# Roadmap: shake-telemetry

This roadmap goes from an empty project to a fully working `shake-telemetry` library as described in PROBLEM.md and ARCHITECTURE.md. Each phase produces a testable milestone. Tasks within a phase are meant to be executed in order.

## Phase 1: Project scaffolding [DONE]

Stand up the Haskell project so it compiles, tests run (empty), and the Nix build works.

### 1.1 Initialize cabal project

Create `shake-telemetry.cabal` with:
- Library component exposing all modules from the architecture's module structure.
- Test suite component using `tasty` as the runner.
- All dependencies from the architecture's dependency table plus test deps (`tasty`, `tasty-hedgehog`, `hedgehog`).
- GHC 9.8.x, `default-language: GHC2021`.

### 1.2 Set up devenv

Create `devenv.nix` (and `devenv.yaml`) providing:
- A dev shell with GHC 9.8.x, cabal, and all Haskell dependencies.
- Any needed pre-commit hooks or scripts.

### 1.3 Create directory structure with module stubs

Create every module from the architecture as a compilable stub:

```
src/Development/Shake/Telemetry.hs
src/Development/Shake/Telemetry/Graph.hs
src/Development/Shake/Telemetry/State.hs
src/Development/Shake/Telemetry/CriticalPath.hs
src/Development/Shake/Telemetry/Json.hs
src/Development/Shake/Telemetry/Mermaid.hs
src/Development/Shake/Telemetry/Wrap/Entry.hs
src/Development/Shake/Telemetry/Wrap/Rules.hs
src/Development/Shake/Telemetry/Wrap/Actions.hs
src/Development/Shake/Telemetry/Wrap/Parallel.hs
src/Development/Shake/Telemetry/Wrap/Reexports.hs
test/Main.hs
```

Each stub should have the correct `module` declaration, minimal imports, and compile cleanly. `test/Main.hs` should run `defaultMain` with an empty test tree.

### 1.4 Create AGENTS.md

Create an `AGENTS.md` file with instructions for LLM agents working on this project: how to build, how to run tests, project conventions, and pointers to the key documents (PROBLEM.md, ARCHITECTURE.md, ROADMAP.md). Sections that can't be filled in yet (e.g., specific test commands, lint steps) should be marked with `TODO` and updated as the project progresses through later phases.

### 1.5 Verify everything compiles

`cabal build` and `cabal test` both succeed inside the devenv shell. This is the baseline for all future work.

---

## Phase 2: Core data model and state management [DONE]

Implement the graph types and the concurrent `TelemetryState`. This phase is pure library code with no Shake dependency -- it can be tested in isolation.

### 2.1 Implement graph types (`Graph.hs`)

Define `NodeType`, `Node`, `Edge`, and `BuildGraph` as specified in the architecture. Derive `Eq`, `Show`, `Generic` for all types. Use strict fields throughout.

### 2.2 Implement TelemetryState (`State.hs`)

Define `TelemetryState` and `newTelemetryState :: IO TelemetryState`. The constructor initializes all `TVar`s to empty and records the monotonic clock baseline.

**Important**: If using STM, use `Control.Concurrent.STM.TVar.Strict` (from `stm`) or the `strict-stm` package to avoid space leaks from unevaluated thunks accumulating inside `TVar`s.

### 2.3 Implement node operations

Implement in `State.hs`:
- `getOrCreateNode :: TelemetryState -> Text -> NodeType -> IO Int` -- Returns existing node ID for a label, or creates a placeholder and returns its ID. This is the deduplication mechanism.
- `registerNode :: TelemetryState -> Text -> NodeType -> IO Int` -- Calls `getOrCreateNode` and records the start time on the node.
- `finishNode :: TelemetryState -> Int -> IO ()` -- Records the end time and computes duration.

All must be atomic (STM transaction or similar).

### 2.4 Implement edge recording

Implement in `State.hs`:
- `recordEdge :: TelemetryState -> Int -> Text -> NodeType -> IO ()` -- Records an edge from the given source node ID to the node identified by the label (creating it via `getOrCreateNode` if needed).

### 2.5 Implement thread context

Implement in `State.hs`:
- `setThreadNode :: TelemetryState -> Int -> IO ()` -- Associates the calling thread's `ThreadId` with the given node ID.
- `getThreadNode :: TelemetryState -> IO Int` -- Returns the node ID for the calling thread. Errors if no context is set (indicates a bug in wrapping).

### 2.6 Implement graph freezing

Implement in `State.hs`:
- `freezeGraph :: TelemetryState -> IO BuildGraph` -- Reads all `TVar`s, converts `IntMap Node` to `Vector Node` and edge list to `Vector Edge`, computes total build duration. Sets `graphCriticalPath` to empty (filled in later by the analysis phase).

### 2.7 Unit tests: state operations

Test with `tasty`:
- Create state, register 3 nodes, record edges between them, freeze the graph. Assert node count, edge count, labels, types.
- Register a node, finish it, verify duration is non-negative.
- Register two nodes with different labels, verify they get different IDs.

### 2.8 Unit tests: node deduplication

- Call `getOrCreateNode` twice with the same label. Verify same ID returned both times.
- Call `recordEdge` to a label that doesn't exist yet, then `registerNode` with the same label. Verify the edge points to the same node that `registerNode` returned.
- Call `registerNode` first, then `recordEdge` to the same label. Verify edge target matches.

### 2.9 Concurrency tests

- Spawn N threads, each registering unique nodes concurrently. Verify total node count equals N after all threads finish.
- Spawn N threads, each recording edges to a shared target label. Verify the target node exists once (dedup works) and all edges point to it.

---

## Phase 3: Critical path analysis [DONE]

Implement the critical path algorithm as a pure function on `BuildGraph`. No Shake involvement.

### 3.1 Implement topological sort

Implement in `CriticalPath.hs`:
- `topologicalSort :: BuildGraph -> [Int]` -- Kahn's algorithm or DFS-based. Returns node IDs in topological order. The graph is a DAG by construction.

### 3.2 Implement longest path DP

Implement in `CriticalPath.hs`:
- `computeCriticalPath :: BuildGraph -> BuildGraph` -- Runs the topological DP algorithm from the architecture doc. Populates `graphCriticalPath` with the ordered list of node IDs forming the longest path. Nodes without timing data (duration = 0 or placeholder) contribute 0 to the path weight.

### 3.3 Unit tests: hand-crafted DAGs

Test with `tasty`:
- **Linear chain**: A -> B -> C with durations 1, 2, 3. Critical path = [A, B, C], total = 6.
- **Diamond**: A -> B, A -> C, B -> D, C -> D. Durations: A=1, B=5, C=2, D=1. Critical path = [A, B, D], total = 7.
- **Wide parallel**: A -> {B, C, D, E} -> F. B is slowest. Critical path goes through B.
- **Single node**: One node, no edges. Critical path = [that node].
- **Disconnected components**: Two independent chains. Critical path is the longer one.

### 3.4 Property tests with hedgehog

- Generate random DAGs (random node count 1-100, random edges respecting topological order, random durations 0.0-10.0).
- Property: the critical path is a valid path in the graph (each consecutive pair has an edge).
- Property: no other path in the graph has a greater total duration (verify against brute-force for small graphs, or verify EFT invariant for larger ones).
- Property: critical path duration <= sum of all node durations.
- Property: all edge endpoints reference existing nodes.

---

## Phase 4: Output formats [DONE]

Implement JSON and Mermaid output as pure functions on `BuildGraph` (post critical-path analysis).

### 4.1 Implement JSON serialization (`Json.hs`)

Define `ToJSON` and `FromJSON` instances for `NodeType`, `Node`, `Edge`, and `BuildGraph` via `aeson`. Match the JSON schema in the architecture doc. Nodes with no timing data have `null` for `startTime`, `endTime`, `duration`.

### 4.2 Implement `writeJsonGraph`

Implement in `Json.hs`:
- `writeJsonGraph :: FilePath -> BuildGraph -> IO ()` -- Writes the graph to the given path as pretty-printed JSON. Creates parent directories if needed.

### 4.3 JSON round-trip tests

- Construct a `BuildGraph` by hand, encode to JSON, decode back, verify equality.
- Verify that a node with no timing data serializes with `null` fields and round-trips correctly.
- Verify the `criticalPath` field appears in the output.
- Verify `onCriticalPath` boolean is set correctly on each node based on `graphCriticalPath`.

### 4.4 Implement Mermaid generation (`Mermaid.hs`)

Implement in `Mermaid.hs`:
- `renderMermaid :: BuildGraph -> Text` -- Produces a Mermaid `graph LR` diagram showing only the critical path subgraph. Each node labeled with its name and duration. All nodes styled with the `critical` class.

### 4.5 Implement `writeMermaidChart`

Implement in `Mermaid.hs`:
- `writeMermaidChart :: FilePath -> BuildGraph -> IO ()` -- Writes the Mermaid text to the given path. Creates parent directories if needed.

### 4.6 Mermaid snapshot tests

- Construct a small `BuildGraph` with a known critical path, render to Mermaid, assert the output matches an expected string (golden test).
- Verify the output is syntactically valid Mermaid (starts with `graph LR`, has `classDef`, has `-->` edges, has `class` statement).

---

## Phase 5: Shake API wrapping [DONE]

Implement all the wrapper functions. Each task covers a logical group of functions. Tests in this phase are unit tests that verify the wrapping mechanics (node/edge recording) using small Shake builds in-process.

### 5.1 Re-exports (`Wrap/Reexports.hs`)

Re-export all ~110 unchanged Shake exports. This is the longest file but purely mechanical: import from `Development.Shake` (qualified) and re-export. Verify it compiles against the Shake API.

### 5.2 Wrap file rule definers (`Wrap/Rules.hs`)

Wrap `(%>)`, `(|%>)`, `(?>)`. Each wraps the user's `FilePath -> Action ()` to call `registerNode` / `setThreadNode` before and `finishNode` after. Test: define a `%>` rule, run it via `shake`, verify a `FileNode` appears in the frozen graph with non-zero duration.

### 5.3 Wrap phony rule definers

Add `phony`, `(~>)`, `phonys` to `Wrap/Rules.hs`. Same wrapping pattern but with `PhonyNode`. Test: define a `phony` rule, verify `PhonyNode` appears.

### 5.4 Wrap oracle rule definers

Add `addOracle`, `addOracleCache`, `addOracleHash` to `Wrap/Rules.hs`. The wrapping must intercept the returned query function to record oracle nodes on query. Test: register an oracle and query it, verify an `OracleNode` appears.

### 5.5 Wrap batch rule definers

Add `(&%>)`, `(&?>)`, `batch` to `Wrap/Rules.hs`. These use `BatchNode`. Test: define a batch rule, verify `BatchNode` appears.

### 5.6 Wrap core dependency creators (`Wrap/Actions.hs`)

Wrap `need`, `needed`, `want`, `orderOnly`, `orderOnlyAction`. Each calls `getThreadNode` to find the source, `recordEdge` for each target, then delegates. Test: a rule that `need`s two files, verify two edges from the rule's node to the needed files.

### 5.7 Wrap oracle dependency creators

Add `askOracle`, `askOracles`, `askOracleWith` to `Wrap/Actions.hs`. Test: `askOracle` inside a rule, verify an `OracleNode` edge is recorded.

### 5.8 Wrap general dependency creators

Add `apply`, `apply1` to `Wrap/Actions.hs`. These are from `Development.Shake.Rule`. Test: use `apply1` with a custom rule key, verify edge is recorded.

### 5.9 Wrap implicit dependency creators

Add `doesFileExist`, `doesDirectoryExist`, `getDirectoryContents`, `getDirectoryFiles`, `getDirectoryDirs`, `getEnv`, `getEnvWithDefault` to `Wrap/Actions.hs`. Test: call `doesFileExist` inside a rule, verify a `DirectoryNode` edge is recorded.

### 5.10 Wrap parallelism (`Wrap/Parallel.hs`)

Wrap `parallel`, `forP`, `par`. Each captures the parent thread's node ID and injects `setThreadNode` at the start of each child action. Test: a rule that calls `parallel [need ["a"], need ["b"]]`, verify both edges are attributed to the parent rule node (not lost or misattributed).

---

## Phase 6: Entry points and drop-in module [DONE]

Wire everything together into the user-facing API.

### 6.1 Implement entry points (`Wrap/Entry.hs`)

Wrap `shake`, `shakeArgs`, `shakeArgsWith`, `shakeArgsOptionsWith`, `shakeWithDatabase`. Each:
1. Creates `TelemetryState` via `newTelemetryState`.
2. Injects it into `shakeExtra` on the provided `ShakeOptions`.
3. Delegates to the real Shake entry point.
4. After the build completes: freezes the graph, computes the critical path, writes JSON and Mermaid outputs.

Output paths default to `_build/telemetry/build-graph.json` and `_build/telemetry/critical-path.mmd`, derived from `shakeFiles` in `ShakeOptions`.

### 6.2 Implement top-level module (`Telemetry.hs`)

`Development.Shake.Telemetry` imports and re-exports:
- Wrapped functions from `Wrap/Entry.hs`, `Wrap/Rules.hs`, `Wrap/Actions.hs`, `Wrap/Parallel.hs`.
- Everything else from `Wrap/Reexports.hs`.

The export list should mirror `Development.Shake` exactly (same names, same types) so the import swap is truly drop-in.

### 6.3 Verify API surface parity

Write a test (or compile-time check) that the set of names exported by `Development.Shake.Telemetry` matches `Development.Shake`. This can be a simple test that imports both modules qualified and uses a representative sample of functions from each to ensure nothing is missing.

---

## Phase 7: Integration testing [DONE]

End-to-end tests that run real Shake builds via `Development.Shake.Telemetry` and verify the full pipeline: telemetry collection, graph construction, critical path analysis, JSON output, and Mermaid output.

### 7.1 Linear chain build

Build with rules A -> B -> C (each `need`s the previous). Verify:
- 3 nodes in the graph, 2 edges.
- Critical path = [C, B, A] (or [A, B, C] depending on edge direction convention).
- JSON file written and parseable.
- Mermaid file written and contains all 3 nodes.

### 7.2 Diamond dependency build

Build with A -> {B, C} -> D. Give B a `threadDelay` to make it slower. Verify:
- 4 nodes, 4 edges.
- Critical path goes through the slow branch (B), not C.

### 7.3 Oracle build

Build with an oracle providing a version string, and a file rule that queries it. Verify:
- Oracle node appears with type `"oracle"`.
- Edge from the file rule to the oracle node exists.

### 7.4 Parallel build

Build with a rule that uses `parallel` to `need` multiple targets. Verify:
- All dependency edges are attributed to the correct parent rule.
- No edges are lost or misattributed.

### 7.5 Batch rules build

Build with batch rules (`&%>`). Verify:
- Batch node appears with type `"batch"`.
- Dependencies are correctly recorded.

### 7.6 Timing plausibility

Across all integration tests, verify:
- All durations are non-negative.
- No node's end time is before its start time.
- The critical path total duration is <= total build time.
- The critical path total duration is >= the duration of any single node.

### 7.7 Idempotent output

Run the same build twice. Verify both runs produce valid JSON and Mermaid output. The graphs may differ in timing but should have the same structure (same nodes and edges).

---

## Phase 8: Cached rule detection [DONE]

Investigated how cached (skipped) rules interact with telemetry. The existing `getOrCreateNode`/`recordEdge` mechanism already handles the most important case: when an executing rule calls `need` on a cached target, `recordEdge` creates a placeholder node with `Nothing` timing via `getOrCreateNode`. No implementation changes were needed — just testing and documenting the behavior.

### 8.1 Detect cached rules via missing timing data

Cached rules that are direct dependencies of executing rules already appear as placeholder nodes with `nodeStartTime = Nothing`, `nodeEndTime = Nothing`, `nodeDuration = Nothing`. This happens because `recordEdge` (called by the wrapped `need`) uses `getOrCreateNode`, which creates the node if it doesn't exist. Since the cached rule's body never runs, `registerNode`/`finishNode` are never called, so the timing fields remain `Nothing`.

### 8.2 Record edges for cached rules

Edges **from** executing rules **to** cached dependencies are recorded (via the executing rule's `need` call). Edges **from** cached rules to their own dependencies are not recorded, because the cached rule's body (containing its `need` calls) never executes. Shake does not expose per-rule skip callbacks or cached dependency information through its public API.

### 8.3 Test: incremental build shows cached nodes

Tested with `alwaysRerun` to force one rule to re-execute while its dependency is cached. Verified:
- The re-executed rule has timing data.
- The cached dependency exists as a placeholder with `nodeDuration = Nothing`.
- The edge between them is present.
- A fully cached build (nothing re-executes) produces an empty graph.

### 8.4 Test: mixed cached and rebuilt

Tested chain A -> B -> C where C re-executes (`alwaysRerun`). Verified:
- C has timing data (re-executed).
- B exists as a placeholder with no timing (direct cached dependency of C).
- A is absent from the graph (transitive dependency through cached B).

### Known limitations

- **Transitive dependencies through cached rules are absent.** If rule C needs B and B needs A, and B is cached, only C and B appear in the graph. A is absent because B's body (containing `need [A]`) never executes.
- **Fully cached builds produce empty graphs.** If nothing re-executes, no telemetry is recorded. The previous build's output files remain available.
- **Shake does not expose per-rule skip callbacks.** The `shakeProgress` API provides aggregate `countSkipped`/`countBuilt` counts but not per-rule information. Solving the above limitations would require Shake upstream changes or database introspection.
