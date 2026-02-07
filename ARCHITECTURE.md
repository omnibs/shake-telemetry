# Architecture: shake-telemetry

## Overview

`shake-telemetry` is a Haskell wrapper library around Shake (the build system) that intercepts dependency-creating function calls to produce a complete build dependency graph with timing data. It computes the critical path and outputs both a machine-readable JSON file and a Mermaid visualization.

Users adopt it by changing one import: `Development.Shake` becomes `Development.Shake.Telemetry`. No other code changes are required.

## Design approach

### Why wrapping works

Every dependency in Shake -- `need`, `askOracle`, `doesFileExist`, `getEnv`, directory queries -- ultimately flows through `apply` / `apply1` from `Development.Shake.Rule`. These functions are the sole mechanism Shake uses to record that the currently executing rule depends on some key.

We cannot intercept `apply` at the internal level without forking Shake. Instead, we wrap each *exported* dependency-creating function: our `need` records the dependency edge, then delegates to Shake's `need`. Since the user's code imports our module, all their calls route through our wrappers.

### State management via shakeExtra

Shake provides `shakeExtra :: HashMap TypeRep Dynamic` on `ShakeOptions`, readable from `Action` via `getShakeExtra` and from `Rules` via `getShakeExtraRules`. Our entry points (`shake`, `shakeArgs`, etc.) inject a `TelemetryState` value into `shakeExtra` before delegating to Shake. All wrappers retrieve it from there -- no global mutable state, no `unsafePerformIO`.

### Thread context tracking

Shake runs rules in parallel across OS threads. When our wrapped `need` records an edge, it must know which rule is currently executing on this thread. We maintain a `TVar (Map ThreadId NodeId)` inside `TelemetryState`. Rule wrappers (`%>`, `phony`, `addOracle`, etc.) register the current thread's node ID before running the action body. Dependency wrappers (`need`, `askOracle`, etc.) look up the current thread's node ID to record the "from" end of each edge.

For `parallel`, `forP`, and `par`, we wrap these to propagate the parent thread's node ID into child threads before they execute.

## Module structure

```
shake-telemetry/
  src/
    Development/Shake/
      Telemetry.hs                  -- Drop-in replacement for Development.Shake
      Telemetry/
        State.hs                    -- TelemetryState, thread context, node registration
        Graph.hs                    -- BuildGraph, Node, Edge, NodeType data types
        CriticalPath.hs             -- Critical path algorithm
        Json.hs                     -- Aeson instances, JSON output
        Mermaid.hs                  -- Mermaid chart generation
        Wrap/
          Entry.hs                  -- Wrapped entry points (shake, shakeArgs, ...)
          Rules.hs                  -- Wrapped rule definers (%>, phony, addOracle, ...)
          Actions.hs                -- Wrapped dependency creators (need, askOracle, ...)
          Parallel.hs               -- Wrapped parallelism (parallel, forP, par)
          Reexports.hs              -- Everything re-exported unchanged from Shake
  test/
    ...
```

### Development.Shake.Telemetry

The top-level module. Exports the full `Development.Shake` API surface. Functions that need telemetry are imported from the `Wrap.*` submodules. Everything else is re-exported from Shake via `Reexports.hs`.

A user's build file changes only this:

```haskell
-- Before
import Development.Shake

-- After
import Development.Shake.Telemetry
```

## Core data model

### Graph types

```haskell
data NodeType
  = FileNode           -- File rules (%>, |%>, ?>)
  | PhonyNode          -- Phony rules (phony, ~>)
  | OracleNode         -- Oracle queries (addOracle, askOracle)
  | BatchNode          -- Batch rules (&%>, &?>)
  | DirectoryNode      -- Directory queries (doesFileExist, getDirectoryFiles, ...)
  | EnvNode            -- Environment lookups (getEnv)
  | ActionNode         -- Root actions (action, want)

data Node = Node
  { nodeId        :: !Int
  , nodeLabel     :: !Text
  , nodeType      :: !NodeType
  , nodeStartTime :: !Double   -- seconds since build start
  , nodeEndTime   :: !Double
  , nodeDuration  :: !Double
  }

data Edge = Edge
  { edgeFrom :: !Int    -- the dependent (the rule that called need)
  , edgeTo   :: !Int    -- the dependency (the file/oracle that was needed)
  }

data BuildGraph = BuildGraph
  { graphNodes        :: !(Vector Node)
  , graphEdges        :: !(Vector Edge)
  , graphBuildStart   :: !UTCTime
  , graphTotalSeconds :: !Double
  , graphCriticalPath :: ![Int]   -- node IDs, populated after analysis
  }
```

### TelemetryState

```haskell
data TelemetryState = TelemetryState
  { tsNodes         :: !(TVar (IntMap Node))
  , tsEdges         :: !(TVar [Edge])
  , tsNextId        :: !(TVar Int)
  , tsThreadContext :: !(TVar (Map ThreadId Int))  -- current node per thread
  , tsBuildStart    :: !TimeSpec                    -- monotonic clock base
  , tsLabelToId     :: !(TVar (HashMap Text Int))  -- dedup: label -> nodeId
  }
```

`tsLabelToId` is needed because a target may appear as a dependency (via `need`) before its rule starts executing. We create a placeholder node on first reference and fill in timing data when the rule runs. This handles the common case where rule A `need`s rule B -- the edge is recorded immediately, and B's node gets timing data when B actually executes.

## Wrapping strategy

### What gets wrapped vs. re-exported

The full `Development.Shake` API (~150 exports) falls into three categories:

#### Wrapped: entry points (5 functions)

These create `TelemetryState`, inject it into `shakeExtra`, delegate to Shake, then write outputs.

| Function | Why wrapped |
|---|---|
| `shake` | Set up + tear down telemetry |
| `shakeArgs` | Same |
| `shakeArgsWith` | Same |
| `shakeArgsOptionsWith` | Same |
| `shakeWithDatabase` | Same (Database entry point) |

#### Wrapped: rule definers (12 functions)

These wrap the user's action body to register a node and set thread context before the body runs, and record end time after.

| Function | Node type |
|---|---|
| `(%>)` | FileNode |
| `(\|%>)` | FileNode |
| `(?>)` | FileNode |
| `(&%>)` | BatchNode |
| `(&?>)` | BatchNode |
| `phony` | PhonyNode |
| `(~>)` | PhonyNode |
| `phonys` | PhonyNode |
| `addOracle` | OracleNode |
| `addOracleCache` | OracleNode |
| `addOracleHash` | OracleNode |
| `batch` | BatchNode |

#### Wrapped: dependency creators (17 functions)

These look up the current thread's node ID and record an edge before delegating.

| Function | Edge target type | Notes |
|---|---|---|
| `need` | FileNode | Core file dependency |
| `needed` | FileNode | Same as need, for already-used files |
| `want` | FileNode | Root targets, edges from ActionNode |
| `orderOnly` | FileNode | Order-only dependency |
| `orderOnlyAction` | (varies) | Action wrapper for order-only deps |
| `askOracle` | OracleNode | Oracle query |
| `askOracles` | OracleNode | Parallel oracle queries |
| `askOracleWith` | OracleNode | Deprecated, but still exported |
| `apply` | (varies) | General dependency mechanism |
| `apply1` | (varies) | Single-key variant |
| `doesFileExist` | DirectoryNode | Implicit dependency |
| `doesDirectoryExist` | DirectoryNode | Implicit dependency |
| `getDirectoryContents` | DirectoryNode | Implicit dependency |
| `getDirectoryFiles` | DirectoryNode | Implicit dependency |
| `getDirectoryDirs` | DirectoryNode | Implicit dependency |
| `getEnv` | EnvNode | Environment variable dependency |
| `getEnvWithDefault` | EnvNode | Same |

#### Wrapped: parallelism (3 functions)

These propagate the thread context to child threads.

| Function | Mechanism |
|---|---|
| `parallel` | Copy parent thread's nodeId to each child thread |
| `forP` | Same |
| `par` | Same |

#### Re-exported unchanged (~110 exports)

Everything else passes through unchanged:
- All types and data constructors (`ShakeOptions`, `Rules`, `Action`, `Verbosity`, `CmdOption`, `Resource`, `Progress`, `ShakeException`, `Rebuild`, `Lint`, `Change`, etc.)
- File operations (`copyFile'`, `readFile'`, `writeFile'`, etc.)
- Temp file/dir operations (`withTempFile`, `withTempDir`, etc.)
- Command execution (`cmd`, `cmd_`, `command`, `command_`, etc.)
- Command result types (`Stdout`, `Stderr`, `Exit`, `CmdTime`, etc.)
- Resource management (`newResource`, `withResource`, etc.)
- Caching (`newCache`, `newCacheIO`)
- Verbosity functions (`getVerbosity`, `putInfo`, etc.)
- Progress reporting (`progressSimple`, `progressDisplay`, etc.)
- FilePattern utilities (`?==`, `<//>`, `filePattern`)
- FilePath utilities (re-exported from `Development.Shake.FilePath`)
- Class re-exports (`ShakeValue`, `RuleResult`, etc.)
- Options accessors (`getShakeOptions`, `shakeOptions`, etc.)
- Rule modifiers (`action`, `alternatives`, `priority`, `versioned`, `withoutActions`)
- Exception handling (`actionOnException`, `actionFinally`, `actionBracket`, `actionCatch`, `actionRetry`)
- Miscellaneous (`alwaysRerun`, `produces`, `historyDisable`, `traced`, `runAfter`, etc.)
- Tracking (`trackRead`, `trackWrite`, `trackAllow`)

## How wrapping works in practice

### Entry point wrapping

```haskell
shake :: ShakeOptions -> Rules () -> IO ()
shake opts rules = do
  state <- newTelemetryState
  let opts' = opts { shakeExtra = addShakeExtra state (shakeExtra opts) }
  Shake.shake opts' rules
  graph <- freezeGraph state
  let analyzed = computeCriticalPath graph
  writeJsonGraph analyzed
  writeMermaidChart analyzed
```

### Rule definer wrapping

```haskell
(%>) :: Located => FilePattern -> (FilePath -> Action ()) -> Rules ()
pat %> act = pat Shake.%> \out -> do
  Just state <- getShakeExtra @TelemetryState
  nodeId <- liftIO $ registerNode state (T.pack out) FileNode
  liftIO $ setThreadNode state nodeId
  act out
  liftIO $ finishNode state nodeId
```

### Dependency creator wrapping

```haskell
need :: Partial => [FilePath] -> Action ()
need files = do
  Just state <- getShakeExtra @TelemetryState
  currentNode <- liftIO $ getThreadNode state
  forM_ files $ \f ->
    liftIO $ recordEdge state currentNode (T.pack f) FileNode
  Shake.need files
```

### Parallelism wrapping

```haskell
parallel :: [Action a] -> Action [a]
parallel actions = do
  Just state <- getShakeExtra @TelemetryState
  parentNode <- liftIO $ getThreadNode state
  Shake.parallel
    [ do liftIO $ setThreadNode state parentNode
         a
    | a <- actions
    ]
```

## Node identity and deduplication

A file like `src/Main.o` may appear in multiple contexts:
1. As a dependency edge target when another rule calls `need ["src/Main.o"]`
2. As a rule node when the `%>` rule for `"src/Main.o"` executes

These must map to the same node. `tsLabelToId` ensures this: the first reference (whether from `need` or `%>`) creates the node, and subsequent references reuse the same ID. The `%>` wrapper fills in timing data when the rule runs.

For oracle nodes, the label is derived from the `Show` instance of the query key: `show queryKey`. For environment nodes, the label is the variable name.

## Critical path algorithm

The critical path through a parallel build is the longest chain of sequential dependencies -- the sequence of rules that, if shortened, would reduce total build time.

### Algorithm (DAG longest path via topological DP)

```
1.  Topological-sort the nodes of the build graph.
2.  For each node v in topological order:
      EFT(v) = duration(v) + max { EFT(u) | u is a predecessor of v }
               (0 if v has no predecessors)
      pred_on_cp(v) = argmax { EFT(u) | u is a predecessor of v }
3.  The critical path ends at:  sink = argmax { EFT(v) | v in all nodes }
4.  Trace back from sink via pred_on_cp to reconstruct the path.
```

This runs in O(V + E) -- trivially fast for 500-5,000 nodes.

### What the critical path tells you

- Rules ON the critical path are bottlenecks: speeding them up reduces total build time.
- Rules OFF the critical path have **slack**: they could take longer without affecting the build.
- The sum of durations along the critical path is the **theoretical minimum build time** given unlimited parallelism.

## Output formats

### JSON

Written to `_build/telemetry/build-graph.json` (configurable via `ShakeOptions`).

```json
{
  "buildStart": "2024-01-15T10:30:00Z",
  "totalSeconds": 120.5,
  "nodes": [
    {
      "id": 1,
      "label": "src/Main.o",
      "type": "file",
      "startTime": 0.0,
      "endTime": 2.5,
      "duration": 2.5,
      "onCriticalPath": true
    }
  ],
  "edges": [
    { "from": 3, "to": 1 }
  ],
  "criticalPath": [4, 1, 7, 12]
}
```

Nodes without timing data (referenced as dependencies but their rule didn't execute in this build -- e.g. up-to-date files Shake skipped) will have `startTime`, `endTime`, and `duration` set to `null`. They still appear as nodes so the graph is complete.

### Mermaid

Written to `_build/telemetry/critical-path.mmd`.

Shows only the critical path subgraph. Each node is labeled with its name and duration. Nodes are styled with a highlight class. Example:

```
graph LR
  classDef critical fill:#ff6b6b,stroke:#333,stroke-width:2px
  n4["compile Main.hs (2.5s)"] --> n1["link (5.0s)"]
  n1 --> n7["copy output (0.3s)"]
  class n4,n1,n7 critical
```

## Testing strategy

### Unit tests

- **Graph construction**: Create `TelemetryState`, call `registerNode` / `recordEdge` / `finishNode` directly, verify the resulting graph.
- **Critical path**: Hand-crafted DAGs with known critical paths. Test diamond graphs, linear chains, wide parallel graphs.
- **Mermaid output**: Snapshot tests of generated Mermaid syntax.
- **JSON output**: Round-trip tests via Aeson (decode . encode == id).
- **Node deduplication**: Verify that `need ["x"]` followed by `"x" %> ...` produces one node, not two.

### Integration tests

- **Small Shake builds**: Actual Shake builds using `Development.Shake.Telemetry` instead of `Development.Shake`. Verify the output graph matches expected dependencies.
- **Parallel correctness**: Build with `parallel` / `forP` and verify edges are attributed to the correct parent rule.
- **Oracle dependencies**: Build with `addOracle` + `askOracle` and verify oracle edges appear in the graph with type `"oracle"`.
- **Timing plausibility**: Verify that recorded durations are non-negative and that the critical path duration ≤ total build time.

### Property tests

- **Graph invariants**: All edge endpoints reference existing nodes. No self-loops. Critical path is a valid path in the graph.
- **Critical path optimality**: For randomly generated DAGs, verify the computed critical path equals the brute-force longest path.

## Dependencies

| Package | Purpose |
|---|---|
| `shake` | The build system being wrapped |
| `aeson` | JSON serialization |
| `text` | Text type for labels |
| `containers` | `IntMap`, `Map` for graph storage |
| `unordered-containers` | `HashMap` for label dedup |
| `vector` | `Vector` for frozen graph output |
| `stm` | `TVar` for thread-safe mutable state |
| `hashable` | `Hashable` for HashMap keys |
| `time` | `UTCTime` for build timestamps |
| `clock` | `TimeSpec` for monotonic timing |

Test dependencies: `tasty`, `tasty-hedgehog`, `hedgehog`.
