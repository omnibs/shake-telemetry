# Thread context loss: path forward

This document describes two complementary approaches to fixing edge loss in shake-telemetry's dependency graph. Option A is a targeted fix for thread context loss. Option B is a longer-term enhancement that uses Shake's own profile data as ground truth for edges.

## Option A: Re-establish thread context after suspending calls

### What it fixes

All thread context loss caused by Shake's continuation-based thread pool requeuing. After this change, edges are no longer dropped when an action resumes on a different thread.

### Mechanism

Every wrapped function that calls a potentially-suspending Shake function saves the current node ID before the call and re-establishes it after the call returns, regardless of which thread the continuation resumed on.

Full design details are in `docs/thread-context-loss-fix.md`.

### Functions to change

**Already wrapped in `Wrap/Actions.hs`** — add `setThreadNode` after each Shake call:

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

**Already wrapped in `Wrap/Parallel.hs`** — add `setThreadNode` after each Shake call:

| Function | Suspends via |
|----------|-------------|
| `parallel` | `actionFenceRequeue` |
| `forP` | `parallel` |
| `par` | `parallel` |

**Already wrapped in `Wrap/Rules.hs`** — intercept the `one` callback in `batch`:

| Function | Suspends via |
|----------|-------------|
| `batch` | `actionFenceRequeue` (between `one` and `many`) |

**Currently re-exported unchanged from `Wrap/Reexports.hs`** — must be moved to wrapped versions:

| Function | Suspends via | Notes |
|----------|-------------|-------|
| `withResource` | `actionFenceRequeueBy` (waiting for resource) | Uses `blockApply` inside — `need` banned within `act`. Only code after `withResource` returns needs context. |
| `withResources` | `withResource` (nested) | Delegates to `withResource` in a fold. Same wrapping pattern. |
| `unsafeExtraThread` | `actionAlwaysRequeue` (after `act` runs) | Uses `blockApply` inside — `need` banned within `act`. Only code after `unsafeExtraThread` returns needs context. |
| `reschedule` | `actionAlwaysRequeuePriority` | No user code runs during suspension. Only code after `reschedule` returns needs context. |
| `newCache` | `actionFenceRequeue` (cache hit waits for in-flight computation) | Returns a `k -> Action v` function. Must wrap the returned function, not `newCache` itself. |
| `newCacheIO` | `actionFenceRequeue` (same) | Same as `newCache` but in `IO`. Wrap the returned function. |

For `withResource`, `withResources`, `unsafeExtraThread`, and `reschedule`, the wrapping pattern is:

```haskell
-- Example: withResource
withResource :: Resource -> Int -> Action a -> Action a
withResource r n act = do
    state <- getTelemetryState
    mNode <- Shake.liftIO $ getThreadNode state
    result <- Shake.withResource r n act
    case mNode of
        Just nid -> Shake.liftIO $ setThreadNode state nid
        Nothing  -> pure ()
    pure result
```

For `newCache` and `newCacheIO`, the returned function must be wrapped:

```haskell
newCacheIO :: (Eq k, Hashable k) => (k -> Action v) -> IO (k -> Action v)
newCacheIO act = do
    cache <- Shake.newCacheIO act
    pure $ \key -> do
        state <- getTelemetryState
        mNode <- Shake.liftIO $ getThreadNode state
        result <- cache key
        case mNode of
            Just nid -> Shake.liftIO $ setThreadNode state nid
            Nothing  -> pure ()
        pure result
```

### Verification

The failing tests from commit `ebda2e5` (blocked sequential `need`, post-`batch` `need`, post-`parallel` `need`) should pass after this change.

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
| 0 | name | `string` | Rule key as produced by `show`. File rules show the filepath (`"src/Main.o"`). Oracle rules show `"OracleQ <question>"`. Directory rules show `"doesFileExist \"path\""`. Environment rules show `"getEnv \"VAR\""`. |
| 1 | execution | `number` | Wall-clock execution time in seconds (e.g. `0.0523`). Zero for cached rules. |
| 2 | built | `int` | Build step number when the rule last executed. `0` means "this run." |
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

| Rule type | Name format | Example |
|-----------|-------------|---------|
| File rule | Filepath | `"src/Main.o"` |
| Phony rule | Target name | `"clean"` |
| Oracle | `"OracleQ " ++ show question` | `"OracleQ (Version ())"` |
| `doesFileExist` | `"doesFileExist \"path\""` | `"doesFileExist \"config.yaml\""` |
| `doesDirectoryExist` | `"doesDirectoryExist \"path\""` | `"doesDirectoryExist \"src\""` |
| `getEnv` | `"getEnv \"VAR\""` | `"getEnv \"HOME\""` |
| `getDirectoryFiles` | `"getDirectoryFiles \"dir\" [pats]"` | `"getDirectoryFiles \"src\" [\"*.hs\"]"` |

### Integration model

1. **Enable `shakeReport`** in the entry point wrappers (`Wrap/Entry.hs`). Write to a temporary path alongside the telemetry output.

2. **Parse the profile JSON** after the build completes, before writing the telemetry graph. Extract entries and their `depends` fields.

3. **Map profile entries to telemetry nodes.** Match by label. File rules match directly (same filepath). Oracle, directory, and environment rules need label normalization (the profile uses Shake's internal `show` format, while our telemetry uses the user-facing label from our wrappers).

4. **Merge edges.** For each profile entry with non-empty `depends`, add edges from that entry's telemetry node to each dependency's telemetry node. Profile-derived edges supplement or replace wrapper-derived edges.

5. **Classify node types.** Profile entries don't carry a type tag. Infer type from the name format:
   - Starts with `"OracleQ "` → `OracleNode`
   - Starts with `"doesFileExist "` or `"doesDirectoryExist "` → `DirectoryNode`
   - Starts with `"getEnv "` → `EnvNode`
   - Starts with `"getDirectoryFiles "` / `"getDirectoryContents "` / `"getDirectoryDirs "` → `DirectoryNode`
   - Otherwise → `FileNode` (or `PhonyNode` if the user's build has registered it as phony)

### Trade-offs

**Benefits:**
- Ground-truth edges — eliminates all edge loss, including through cached rules
- No Shake fork
- Uses data Shake already produces

**Costs:**
- Profile JSON format is positional and undocumented — stable in practice but not a typed API contract
- Label mapping between profile format and telemetry format requires normalization rules
- Adds a post-build parsing step and file I/O
- Node type inference from name strings is heuristic (but can be supplemented by wrapper-recorded types)

### Scope

| Component | Change |
|-----------|--------|
| New module: `Telemetry/Profile.hs` | Parse Shake profile JSON, extract entries and edges |
| `Wrap/Entry.hs` | Enable `shakeReport` to a temp file, call profile parser after build |
| `State.hs` or `Graph.hs` | Merge profile-derived edges into `BuildGraph` |
| `shake-telemetry.cabal` | No new dependencies (aeson already used) |

---

## Sequencing

**Option A first.** It's a small, mechanical change (~50 lines across 3-4 files) that fixes the immediate problem. The existing failing tests validate it. No new modules or parsing infrastructure needed.

**Option B second.** It's a larger effort (new module, JSON parsing, label normalization, merge logic) that provides a qualitative improvement: ground-truth edges including through cached rules. It builds on Option A — the wrapper-recorded node types and timing enrichment remain valuable even when edges come from the profile.

After both are implemented, the telemetry pipeline is:
1. Wrappers record node metadata (type, label, timing) and best-effort edges
2. Shake profile provides authoritative edges
3. Merge step combines both: profile edges as primary source, wrapper edges as fallback for any entries not in the profile
