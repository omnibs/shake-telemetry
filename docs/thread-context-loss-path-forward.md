# Thread context loss: path forward

This document describes two complementary approaches to fixing edge loss in shake-telemetry's dependency graph. Option A is a targeted fix for thread context loss. Option B is a longer-term enhancement that uses Shake's own profile data as ground truth for edges.

## Option A: Re-establish thread context after suspending calls

### What it fixes

Thread context loss caused by Shake's continuation-based thread pool requeuing, for all paths through the wrapped API. After this change, edges are no longer dropped when an action resumes on a different thread, provided the user imports `Development.Shake.Telemetry` exclusively (the intended usage). Callers that bypass the telemetry module and use raw `Development.Shake` functions directly are not covered — this is inherent to the wrapper-based design.

### Mechanism

Every wrapped function that calls a potentially-suspending Shake function saves the current node ID before the call and re-establishes it after the call returns, regardless of which thread the continuation resumed on.

The re-establishment must be **exception-safe**: if the Shake call throws (e.g. a dependency fails to build) and user code catches the exception via `actionCatch`, context must still be restored so that subsequent calls in the catch handler or after it can record edges. This requires using `actionFinally` rather than sequencing the restore after the call.

Full design details are in `docs/thread-context-loss-fix.md`.

### The pattern

The core wrapping pattern for all suspending calls:

```haskell
need :: HasCallStack => [FilePath] -> Action ()
need files = do
    state <- getTelemetryState
    mNode <- Shake.liftIO $ getThreadNode state
    case mNode of
        Nothing -> Shake.need files  -- already lost, nothing to save
        Just nid -> do
            mapM_ (\f -> Shake.liftIO $ recordEdge state nid (T.pack f) FileNode) files
            -- actionFinally ensures context is restored even if Shake.need throws.
            -- If user code catches the exception (actionCatch), subsequent calls
            -- will still find valid context.
            Shake.need files
                `Shake.actionFinally` setThreadNode state nid
```

Using `Shake.actionFinally :: Action a -> IO b -> Action a` guarantees `setThreadNode` runs on the current thread whether the call succeeds or throws. Since `setThreadNode` is `IO`, it fits `actionFinally`'s cleanup parameter directly.

### Functions to change

**Already wrapped in `Wrap/Actions.hs`** — add `actionFinally`-based `setThreadNode` after each Shake call:

| Function | Suspends via |
|----------|-------------|
| `need` | `apply` |
| `needed` | `apply` |
| `orderOnly` | `apply` |
| `askOracle` | `apply1` |
| `askOracles` | `apply` |
| `askOracleWith` | `apply1` |
| `doesFileExist` | `apply1` |
| `doesDirectoryExist` | `apply1` |
| `getDirectoryContents` | `apply1` |
| `getDirectoryFiles` | `apply1` |
| `getDirectoryDirs` | `apply1` |
| `getEnv` | `apply1` |
| `getEnvWithDefault` | `apply1` |

**Already wrapped in `Wrap/Parallel.hs`** — add `actionFinally`-based `setThreadNode` after each Shake call:

| Function | Suspends via |
|----------|-------------|
| `parallel` | `actionFenceRequeue` |
| `forP` | `parallel` |
| `par` | `parallel` |

**Already wrapped in `Wrap/Rules.hs`** — intercept the `one` callback in `batch`:

| Function | Suspends via |
|----------|-------------|
| `batch` | `actionFenceRequeue` (between `one` and `many`) |

The `one` callback restore should also use `actionFinally`:

```haskell
wrappedRules realOne = rules $ \a -> do
    state <- getTelemetryState
    mNode <- Shake.liftIO $ getThreadNode state
    case mNode of
        Nothing -> realOne a
        Just nid ->
            realOne a
                `Shake.actionFinally` setThreadNode state nid
```

**Currently re-exported unchanged from `Wrap/Reexports.hs`** — must be moved to wrapped versions:

| Function | Suspends via | Notes |
|----------|-------------|-------|
| `withResource` | `actionFenceRequeueBy` (waiting for resource) | Uses `blockApply` inside — `need` banned within `act`. Only code after `withResource` returns needs context. |
| `withResources` | `withResource` (nested) | Delegates to `withResource` in a fold. Same wrapping pattern. |
| `unsafeExtraThread` | `actionAlwaysRequeue` (after `act` runs) | Uses `blockApply` inside — `need` banned within `act`. Only code after `unsafeExtraThread` returns needs context. |
| `reschedule` | `actionAlwaysRequeuePriority` | No user code runs during suspension. Only code after `reschedule` returns needs context. |
| `newCache` | `actionFenceRequeue` (cache hit waits for in-flight computation) | Returns a `k -> Action v` function. Must wrap the returned function, not `newCache` itself. |
| `newCacheIO` | `actionFenceRequeue` (same) | Same as `newCache` but in `IO`. Wrap the returned function. |

For `withResource`, `withResources`, `unsafeExtraThread`, and `reschedule`:

```haskell
-- Example: withResource
withResource :: Resource -> Int -> Action a -> Action a
withResource r n act = do
    state <- getTelemetryState
    mNode <- Shake.liftIO $ getThreadNode state
    case mNode of
        Nothing -> Shake.withResource r n act
        Just nid ->
            Shake.withResource r n act
                `Shake.actionFinally` setThreadNode state nid
```

For `newCache` and `newCacheIO`, the returned function must be wrapped:

```haskell
newCacheIO :: (Eq k, Hashable k) => (k -> Action v) -> IO (k -> Action v)
newCacheIO act = do
    cache <- Shake.newCacheIO act
    pure $ \key -> do
        state <- getTelemetryState
        mNode <- Shake.liftIO $ getThreadNode state
        case mNode of
            Nothing -> cache key
            Just nid ->
                cache key
                    `Shake.actionFinally` setThreadNode state nid
```

### Verification

**Existing tests:** The failing tests from commit `ebda2e5` (blocked sequential `need`, post-`batch` `need`, post-`parallel` `need`) should pass after this change.

**New tests required:**

| Test | What it validates |
|------|-------------------|
| `withResource` context preservation | Rule calls `withResource`, then `need` after it returns. Verify edge recorded. |
| `withResources` context preservation | Same as above with multiple resources. |
| `unsafeExtraThread` context preservation | Rule calls `unsafeExtraThread` with a `cmd` call, then `need` after. Verify edge recorded. |
| `reschedule` context preservation | Rule calls `reschedule`, then `need` after. Verify edge recorded. |
| `newCache`/`newCacheIO` context preservation | Two rules query the same cache (second hits fence path). Verify edges recorded for both callers. |
| Exception + `actionCatch` context preservation | Rule calls `need ["nonexistent"]` inside `actionCatch`, then calls `need ["real-dep"]` after the catch. Verify the second edge is recorded despite the first call throwing. |

### What Option A does NOT fix

Cached/skipped rules. When Shake skips a rule, its body never executes, so our wrappers never fire. Edges through cached rules are absent from the graph. This is inherent to the wrapper approach and is addressed by Option B.

---

## Option B: Merge edges from Shake's profile report

### What it fixes

Edge completeness for all rules, including cached rules and any edge case where wrapper-based tracking is incomplete. Shake internally records exact, correct dependencies in its database, and exposes them via the profile report.

### Motivation

Shake already tracks the precise dependency graph in its rule database. Every rule's dependencies are stored as part of its result, regardless of whether the rule executed or was cached. The profile report (enabled via `shakeReport`) dumps this data to JSON. By parsing this report and merging its edges into our telemetry graph, we get ground-truth dependency information that is:

- Action-local, not thread-local (immune to context loss by construction)
- Complete for cached rules (dependencies stored from last execution)
- Authoritative (this is Shake's own source of truth for rebuild decisions)

### Shake profile JSON schema

The report is generated by `Development.Shake.Internal.Profile.generateJSON`. The top-level structure is a JSON array of entries. **Each entry is a positional array, not a named object.**

```
[
  [name, execution, built, changed, depends?, traces?]
, [name, execution, built, changed, depends?, traces?]
, ...
]
```

#### Entry fields (by position)

| Index | Field | Type | Description |
|-------|-------|------|-------------|
| 0 | name | `string` | Rule key as produced by `show`. See "Name format by rule type" below. |
| 1 | execution | `number` | Wall-clock execution time in seconds (e.g. `0.0523`). Zero for cached rules. |
| 2 | built | `int` | Build step number when the rule last executed. `0` means "this run." (aka `prfBuilt`) |
| 3 | changed | `int` | Build step number when the rule's result last changed. |
| 4 | depends | `[[int]]` | **Optional.** Dependency groups. Each group is a list of 0-based indices into the top-level array. Omitted when both `depends` and `traces` are empty. |
| 5 | traces | `[[string, number, number]]` | **Optional.** Traced commands. Each trace is `[command, start_seconds, stop_seconds]`. Omitted when empty. |

#### Dependency groups

The `depends` field is `[[int]]`, a list of lists. Each inner list corresponds to one `apply`/`need` call and contains the indices of the rules that were depended on. For example:

```json
[[0, 2, 3], [5]]
```

This means the rule made two dependency calls:
- First call depended on entries 0, 2, and 3 (e.g. `need ["a.o", "b.o", "c.o"]`)
- Second call depended on entry 5 (e.g. `askOracle someQuery`)

#### Example

```json
[
  ["a.o",    0,      1, 1]
, ["b.o",    0,      1, 1]
, ["c.o",    0.001,  0, 0, [[0, 1]]]
, ["output", 0.523,  0, 0, [[2], [4]], [["gcc -o output c.o", 0.1, 0.62]]]
, ["OracleQ (Version ())", 0.002, 0, 0]
]
```

Reading entry 3 (`"output"`):
- Took 0.523s to execute
- Built and changed this run (step 0)
- First dep group `[2]`: depends on entry 2 (`"c.o"`)
- Second dep group `[4]`: depends on entry 4 (`"OracleQ (Version ())"`)
- One traced command: `gcc -o output c.o` from 0.1s to 0.62s

Entry 0 and 1 (`"a.o"`, `"b.o"`) have no `depends` field — they are source files or roots with no dependencies.

#### Name format by rule type

Names are produced by Shake's `show` instances. The argument portion uses `wrapQuote`, which only adds quotes when the string contains whitespace. Classifiers should match on the stable prefix, not on quoting style.

| Rule type | Stable prefix | Example (no spaces) | Example (with spaces) |
|-----------|--------------|--------------------|-----------------------|
| File rule | *(none — bare filepath)* | `src/Main.o` | `my file.o` |
| Phony rule | *(none — bare target name)* | `clean` | `run tests` |
| Oracle | `OracleQ ` | `OracleQ (Version ())` | |
| `doesFileExist` | `doesFileExist ` | `doesFileExist config.yaml` | `doesFileExist "my config.yaml"` |
| `doesDirectoryExist` | `doesDirectoryExist ` | `doesDirectoryExist src` | `doesDirectoryExist "my dir"` |
| `getEnv` | `getEnv ` | `getEnv HOME` | `getEnv "MY VAR"` |
| `getDirectoryFiles` | `getDirectoryFiles ` | `getDirectoryFiles src [*.hs]` | |
| `getDirectoryContents` | `getDirectoryContents ` | `getDirectoryContents src` | |
| `getDirectoryDirs` | `getDirectoryDirs ` | `getDirectoryDirs src` | |

### Enabling shakeReport

The entry point wrappers (`Wrap/Entry.hs`) must enable profile JSON generation without clobbering user-configured reports. `ShakeOptions.shakeReport :: [FilePath]` is a list of report paths. The integration must **append** a temporary JSON path to the user's configured reports.

The append must happen on the **final resolved `ShakeOptions`** — after CLI processing and any user callback adjustments. For `shake opts rules`, we control `opts` directly. But for `shakeArgsWith` and `shakeArgsOptionsWith`, CLI arguments (e.g. `--report=FILE`) and the user's option-processing callback can modify `shakeReport` after our initial setup. If we append to the initial `opts` and CLI processing later replaces `shakeReport`, our path is lost.

Concretely, for `shakeArgsWith`-style entry points, the temp path must be appended inside or after the user's option callback, on the options object that is actually passed to the build runner:

```haskell
-- In the shakeArgsWith wrapper, after user callback processes options:
let tempReport = telemetryDir </> ".shake-profile.json"
let finalOpts = userProcessedOpts { shakeReport = shakeReport userProcessedOpts ++ [tempReport] }
```

This preserves:
- User-configured `shakeReport` paths (e.g. `["my-report.html"]`)
- CLI-provided `--report=FILE` arguments
- Any modifications from the user's option-processing callback

The temporary report file should be cleaned up after parsing, or written to the telemetry output directory where it won't surprise users.

The same final-resolved-options principle applies to **all telemetry output paths** — the JSON graph, Mermaid chart, and temp profile path are all derived from `shakeFiles` in the options. If a user callback changes `shakeFiles`, all derived paths must use the changed value. This is already the concern for `shakeReport` above; it just needs to be applied consistently across all path derivations in `Wrap/Entry.hs`.

### Filtering to the current build closure

Shake's profile database is persistent across builds. A profile dump includes all rules in the database, not just those relevant to the current run. On an incremental build that rebuilds 3 out of 5000 rules, the profile contains all 5000 entries.

The merge step must filter to the **transitive dependency closure of the current run's roots**:

1. Look for a synthetic `Root` entry (Shake's internal root key, `Development.Shake.Internal.Core.Types.Root`). If present, seed the traversal from it — its dependencies are the user-requested targets.
2. If no `Root` entry is found, fall back to seeding from all entries with `prfBuilt == 0` (built in this run).
3. Starting from the seed entries, follow `prfDepends` transitively to include all reachable entries (including cached rules with `prfBuilt > 0`).
4. Discard all entries not reachable from the seed set.

Seeding from the `Root` entry is more precise — it identifies exactly the targets the user requested, rather than all rules that happened to execute. In practice the results are equivalent (every `prfBuilt == 0` entry is reachable from `Root`), but the `Root`-first approach is more obviously correct and self-documenting. Whether the `Root` entry appears in the profile JSON should be verified during implementation.

### Integration model

1. **Append a temp JSON report path** to `shakeReport` in the entry point wrappers, preserving any user-configured report paths.

2. **Parse the profile JSON** after the build completes, before writing the telemetry graph. **Parser failures are non-fatal.** If the temp report is missing (build was interrupted before Shake wrote it), malformed (Shake version changed the format), or partially written (Ctrl-C during write), the parser should log a warning and fall back to wrapper-only edges. Telemetry must never affect the build or crash the post-build output step.

3. **Filter to current build closure** using the Root-seeded (or `prfBuilt == 0` fallback) transitive reachability algorithm described above.

4. **Map profile entries to telemetry nodes.** Match by label. File rules match directly (same filepath). Oracle, directory, and environment rules need label normalization (the profile uses Shake's internal `show` format, while our telemetry uses the user-facing label from our wrappers).

5. **Merge edges.** For each profile entry in the closure with non-empty `depends`, add edges from that entry's telemetry node to each dependency's telemetry node. The final edge set is **deduplicated by `(from, to)` pair** — if both the wrapper and the profile recorded the same edge, only one copy appears in the graph. After deduplication, edges should be **sorted by `(from, to)`** for deterministic output across runs (edge insertion order currently depends on thread scheduling). Currently `recordEdge` (`State.hs`) appends to a list without deduplication, and `freezeGraph` converts to a `Vector` as-is. The deduplication and sorting step should happen after merging, before writing the final `BuildGraph`.

6. **Classify node types** for profile-only entries (rules not seen by wrappers, e.g. cached rules). Infer type from the stable name prefix:
   - Starts with `OracleQ ` → `OracleNode`
   - Starts with `doesFileExist ` or `doesDirectoryExist ` → `DirectoryNode`
   - Starts with `getEnv ` → `EnvNode`
   - Starts with `getDirectoryFiles ` / `getDirectoryContents ` / `getDirectoryDirs ` → `DirectoryNode`
   - Otherwise → `FileNode` (or `PhonyNode` if the wrapper layer registered it as phony)

### Trade-offs

**Benefits:**
- Ground-truth edges — eliminates all edge loss, including through cached rules
- No Shake fork
- Uses data Shake already produces

**Costs:**
- Profile JSON format is positional and undocumented — stable in practice but not a typed API contract
- Label mapping between profile format and telemetry format requires normalization rules
- Adds a post-build parsing step and file I/O
- Node type inference from name prefixes is heuristic (but can be supplemented by wrapper-recorded types)

### Scope

| Component | Change |
|-----------|--------|
| New module: `Telemetry/Profile.hs` | Parse Shake profile JSON, filter to build closure, extract entries and edges |
| `Wrap/Entry.hs` | Append temp JSON report path to `shakeReport`, call profile parser after build |
| `State.hs` or `Graph.hs` | Merge profile-derived edges into `BuildGraph` |
| `shake-telemetry.cabal` | No new dependencies (aeson already used) |

---

## Sequencing

**Option A first.** It's a small, mechanical change (~60 lines across 4 files) that fixes the immediate problem. The existing failing tests validate the core mechanism, and new tests cover the additional wrapped functions and exception safety. No new modules or parsing infrastructure needed.

**Option B second.** It's a larger effort (new module, JSON parsing, build closure filtering, label normalization, merge logic) that provides a qualitative improvement: ground-truth edges including through cached rules. It builds on Option A — the wrapper-recorded node types and timing enrichment remain valuable even when edges come from the profile.

After both are implemented, the telemetry pipeline is:
1. Wrappers record node metadata (type, label, timing) and best-effort edges
2. Shake profile provides authoritative edges for the current build closure
3. Merge step combines both, deduplicates by `(from, to)` pair, and writes the final graph
