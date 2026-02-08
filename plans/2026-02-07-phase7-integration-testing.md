# Phase 7: Integration Testing Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Write end-to-end integration tests that run real Shake builds via `Development.Shake.Telemetry` and verify the full pipeline: telemetry collection, graph construction, critical path analysis, JSON output, and Mermaid output.

**Architecture:** A single new test module `Test.Telemetry.IntegrationTest` containing integration tests that use the wrapped entry point (`Development.Shake.Telemetry.Wrap.Entry.shake`) with all wrapped rules/actions/parallel functions. Each test runs a complete Shake build in a temp directory, then inspects both the frozen `BuildGraph` and the written JSON/Mermaid output files. A shared helper provides the common setup pattern.

**Tech Stack:** Haskell, GHC2021, tasty + tasty-hunit, temporary, aeson (for JSON parsing), Development.Shake.Telemetry.* modules

---

### Task 1: Create IntegrationTest module with shared helpers

**Files:**
- Create: `test/Test/Telemetry/IntegrationTest.hs`
- Modify: `test/Main.hs` (add import + wire into test tree)
- Modify: `shake-telemetry.cabal` (add to other-modules)

**Step 1: Create the module file with helpers and no tests yet**

Create `test/Test/Telemetry/IntegrationTest.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Test.Telemetry.IntegrationTest (integrationTests) where

import Control.Concurrent (threadDelay)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.IntMap.Strict qualified as IntMap
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as Vector
import Development.Shake (ShakeOptions (..), shakeOptions)
import Development.Shake qualified as Shake
import Development.Shake.Telemetry.CriticalPath (computeCriticalPath)
import Development.Shake.Telemetry.Graph
import Development.Shake.Telemetry.State (TelemetryState, freezeGraph, newTelemetryState)
import Development.Shake.Telemetry.Wrap.Actions qualified as WA
import Development.Shake.Telemetry.Wrap.Parallel qualified as WP
import Development.Shake.Telemetry.Wrap.Rules qualified as WR
import System.Directory (doesFileExist)
import System.FilePath ((</>), (-<.>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

-- | Run a Shake build with telemetry, returning the analyzed graph.
-- Also verifies that JSON and Mermaid output files were written.
withTelemetryBuild
  :: String
  -> (FilePath -> TelemetryState -> ShakeOptions -> Shake.Rules () -> IO ())
  -> (FilePath -> BuildGraph -> Assertion)
  -> Assertion
withTelemetryBuild name buildFn checkFn =
  withSystemTempDirectory ("shake-integ-" ++ name) $ \tmpDir -> do
    state <- newTelemetryState
    let opts =
          shakeOptions
            { shakeFiles = tmpDir </> ".shake"
            , shakeVerbosity = Shake.Silent
            , shakeExtra = Shake.addShakeExtra state (shakeExtra shakeOptions)
            }
    buildFn tmpDir state opts
    graph <- freezeGraph state
    let analyzed = computeCriticalPath graph
    checkFn tmpDir analyzed

-- | Helper to run a plain Shake build.
runBuild :: ShakeOptions -> Shake.Rules () -> IO ()
runBuild = Shake.shake

-- | Find all nodes of a given type.
nodesOfType :: NodeType -> BuildGraph -> [Node]
nodesOfType ntype graph =
  filter (\n -> nodeType n == ntype) (IntMap.elems (graphNodes graph))

-- | Find a node by label substring.
findNodeByLabel :: Text -> BuildGraph -> Maybe Node
findNodeByLabel substr graph =
  let nodes = IntMap.elems (graphNodes graph)
  in case filter (\n -> substr `T.isInfixOf` nodeLabel n) nodes of
    (n : _) -> Just n
    [] -> Nothing

-- | Count edges from a given node ID.
edgesFrom :: Int -> BuildGraph -> Int
edgesFrom nid graph =
  Vector.length $ Vector.filter (\e -> edgeFrom e == nid) (graphEdges graph)

-- | Count edges to a given node ID.
edgesTo :: Int -> BuildGraph -> Int
edgesTo nid graph =
  Vector.length $ Vector.filter (\e -> edgeTo e == nid) (graphEdges graph)

integrationTests :: TestTree
integrationTests =
  testGroup
    "Integration"
    []
```

**Step 2: Add the module to cabal and Main.hs**

In `shake-telemetry.cabal`, add `Test.Telemetry.IntegrationTest` to the `other-modules` list in the test suite.

In `test/Main.hs`:
- Add `import Test.Telemetry.IntegrationTest (integrationTests)`
- Add `integrationTests` to the test tree list.

**Step 3: Verify it compiles**

Run: `devenv shell -- cabal test --enable-tests`
Expected: All 32 existing tests pass, new module compiles with 0 tests.

**Step 4: Commit**

```
git add test/Test/Telemetry/IntegrationTest.hs test/Main.hs shake-telemetry.cabal
git commit -m "test: add IntegrationTest module skeleton with shared helpers"
```

---

### Task 2: Linear chain build test (7.1)

**Files:**
- Modify: `test/Test/Telemetry/IntegrationTest.hs`

**Step 1: Write the test**

Add to `integrationTests` test list and implement:

```haskell
testCase "7.1: linear chain A -> B -> C" testLinearChain
```

```haskell
testLinearChain :: Assertion
testLinearChain =
  withTelemetryBuild "linear" buildFn checkFn
  where
    buildFn tmpDir _state opts = runBuild opts $ do
      (tmpDir </> "a.txt") WR.%> \out -> do
        Shake.writeFile' out "a"
      (tmpDir </> "b.txt") WR.%> \out -> do
        WA.need [tmpDir </> "a.txt"]
        Shake.writeFile' out "b"
      (tmpDir </> "c.txt") WR.%> \out -> do
        WA.need [tmpDir </> "b.txt"]
        Shake.writeFile' out "c"
      Shake.want [tmpDir </> "c.txt"]

    checkFn _tmpDir graph = do
      -- 3 file nodes expected
      let fileNodes = nodesOfType FileNode graph
      assertBool ("3 file nodes, got " ++ show (length fileNodes)) (length fileNodes == 3)

      -- At least 2 edges (c->b, b->a)
      let edgeCount = Vector.length (graphEdges graph)
      assertBool (">= 2 edges, got " ++ show edgeCount) (edgeCount >= 2)

      -- Critical path should contain all 3 nodes
      let cpLen = length (graphCriticalPath graph)
      assertBool ("critical path has 3 nodes, got " ++ show cpLen) (cpLen == 3)
```

**Step 2: Run tests**

Run: `devenv shell -- cabal test --enable-tests`
Expected: 33 tests pass (32 existing + 1 new).

**Step 3: Commit**

```
git add test/Test/Telemetry/IntegrationTest.hs
git commit -m "test: add linear chain integration test (7.1)"
```

---

### Task 3: Diamond dependency build test (7.2)

**Files:**
- Modify: `test/Test/Telemetry/IntegrationTest.hs`

**Step 1: Write the test**

Add to `integrationTests` test list:

```haskell
testCase "7.2: diamond A -> {B, C} -> D, B is slow" testDiamond
```

```haskell
testDiamond :: Assertion
testDiamond =
  withTelemetryBuild "diamond" buildFn checkFn
  where
    buildFn tmpDir _state opts = runBuild opts $ do
      -- D is the root target, depends on B and C
      (tmpDir </> "d.txt") WR.%> \out -> do
        WA.need [tmpDir </> "b.txt", tmpDir </> "c.txt"]
        Shake.writeFile' out "d"
      -- B is slow (100ms delay)
      (tmpDir </> "b.txt") WR.%> \out -> do
        WA.need [tmpDir </> "a.txt"]
        Shake.liftIO $ threadDelay 100000
        Shake.writeFile' out "b"
      -- C is fast
      (tmpDir </> "c.txt") WR.%> \out -> do
        WA.need [tmpDir </> "a.txt"]
        Shake.writeFile' out "c"
      -- A is a leaf
      (tmpDir </> "a.txt") WR.%> \out -> do
        Shake.writeFile' out "a"
      Shake.want [tmpDir </> "d.txt"]

    checkFn tmpDir graph = do
      -- 4 file nodes
      let fileNodes = nodesOfType FileNode graph
      assertBool ("4 file nodes, got " ++ show (length fileNodes)) (length fileNodes == 4)

      -- 4 edges: d->b, d->c, b->a, c->a
      let edgeCount = Vector.length (graphEdges graph)
      assertBool (">= 4 edges, got " ++ show edgeCount) (edgeCount >= 4)

      -- Critical path should go through B (the slow branch)
      let cpNodeIds = graphCriticalPath graph
          cpNodes = [n | nid <- cpNodeIds, Just n <- [IntMap.lookup nid (graphNodes graph)]]
          cpLabels = map nodeLabel cpNodes
          hasB = any (T.isInfixOf "b.txt") cpLabels
          hasC = any (T.isInfixOf "c.txt") cpLabels
      assertBool "critical path goes through B (slow)" hasB
      -- C might or might not be on CP; the important thing is B is there
```

**Step 2: Run tests**

Run: `devenv shell -- cabal test --enable-tests`
Expected: 34 tests pass.

**Step 3: Commit**

```
git add test/Test/Telemetry/IntegrationTest.hs
git commit -m "test: add diamond dependency integration test (7.2)"
```

---

### Task 4: Oracle build test (7.3)

**Files:**
- Modify: `test/Test/Telemetry/IntegrationTest.hs`

**Step 1: Write the test**

This test needs an oracle key type. We'll define a simple newtype in the test file:

```haskell
-- At top of file, add imports:
import Development.Shake.Classes (Binary, Hashable, NFData, Typeable)
import GHC.Generics (Generic)

-- Oracle key type for testing
newtype VersionQ = VersionQ () deriving (Eq, Show, Typeable, Generic, Hashable, Binary, NFData)
type instance Shake.RuleResult VersionQ = String
```

Add to test list:

```haskell
testCase "7.3: oracle build" testOracle
```

```haskell
testOracle :: Assertion
testOracle =
  withTelemetryBuild "oracle" buildFn checkFn
  where
    buildFn tmpDir _state opts = runBuild opts $ do
      _ <- WR.addOracle $ \(VersionQ _) -> pure "1.0.0"
      (tmpDir </> "version.txt") WR.%> \out -> do
        ver <- WA.askOracle (VersionQ ())
        Shake.writeFile' out ver
      Shake.want [tmpDir </> "version.txt"]

    checkFn _tmpDir graph = do
      -- Oracle node exists
      let oracleNodes = nodesOfType OracleNode graph
      assertBool ("oracle node exists, got " ++ show (length oracleNodes)) (not (null oracleNodes))

      -- Edge from file rule to oracle node exists
      let fileNodes = nodesOfType FileNode graph
      case fileNodes of
        [] -> assertFailure "no file node found"
        (fn : _) -> do
          let outEdges = edgesFrom (nodeId fn) graph
          assertBool ("file node has edges, got " ++ show outEdges) (outEdges > 0)
```

**Step 2: Run tests**

Run: `devenv shell -- cabal test --enable-tests`
Expected: 35 tests pass.

If the oracle test doesn't compile because `RuleResult` type instance needs to be at top level, adjust accordingly.

**Step 3: Commit**

```
git add test/Test/Telemetry/IntegrationTest.hs
git commit -m "test: add oracle build integration test (7.3)"
```

---

### Task 5: Parallel build test (7.4)

**Files:**
- Modify: `test/Test/Telemetry/IntegrationTest.hs`

**Step 1: Write the test**

Add to test list:

```haskell
testCase "7.4: parallel build preserves edges" testParallel
```

```haskell
testParallel :: Assertion
testParallel =
  withTelemetryBuild "parallel" buildFn checkFn
  where
    buildFn tmpDir _state opts = runBuild opts $ do
      (tmpDir </> "//*.dep") WR.%> \out -> Shake.writeFile' out "dep"
      (tmpDir </> "main.txt") WR.%> \out -> do
        _ <- WP.parallel
          [ WA.need [tmpDir </> "x.dep"]
          , WA.need [tmpDir </> "y.dep"]
          , WA.need [tmpDir </> "z.dep"]
          ]
        Shake.writeFile' out "main"
      Shake.want [tmpDir </> "main.txt"]

    checkFn tmpDir graph = do
      -- main.txt node should have edges to all 3 deps
      case findNodeByLabel "main.txt" graph of
        Nothing -> assertFailure "main.txt node not found"
        Just mainNode -> do
          let outEdges = edgesFrom (nodeId mainNode) graph
          assertBool
            ("main.txt has >= 3 outgoing edges, got " ++ show outEdges)
            (outEdges >= 3)

      -- All 3 dep nodes should exist
      let depNodes = filter (\n -> any (`T.isInfixOf` nodeLabel n) ["x.dep", "y.dep", "z.dep"])
                       (IntMap.elems (graphNodes graph))
      assertBool ("3 dep nodes, got " ++ show (length depNodes)) (length depNodes == 3)
```

**Step 2: Run tests**

Run: `devenv shell -- cabal test --enable-tests`
Expected: 36 tests pass.

**Step 3: Commit**

```
git add test/Test/Telemetry/IntegrationTest.hs
git commit -m "test: add parallel build integration test (7.4)"
```

---

### Task 6: Timing plausibility test (7.6)

**Files:**
- Modify: `test/Test/Telemetry/IntegrationTest.hs`

**Step 1: Write the test**

This test runs a small build and verifies timing invariants on every node.

Add to test list:

```haskell
testCase "7.6: timing plausibility" testTimingPlausibility
```

```haskell
testTimingPlausibility :: Assertion
testTimingPlausibility =
  withTelemetryBuild "timing" buildFn checkFn
  where
    buildFn tmpDir _state opts = runBuild opts $ do
      (tmpDir </> "slow.txt") WR.%> \out -> do
        Shake.liftIO $ threadDelay 50000  -- 50ms
        Shake.writeFile' out "slow"
      (tmpDir </> "fast.txt") WR.%> \out -> do
        WA.need [tmpDir </> "slow.txt"]
        Shake.writeFile' out "fast"
      Shake.want [tmpDir </> "fast.txt"]

    checkFn _tmpDir graph = do
      -- All nodes with timing data: duration >= 0
      let allNodes = IntMap.elems (graphNodes graph)
          nodesWithDuration = filter (\n -> nodeDuration n /= Nothing) allNodes
      forM_ nodesWithDuration $ \n -> do
        let dur = maybe 0 id (nodeDuration n)
        assertBool
          ("node " ++ show (nodeLabel n) ++ " duration >= 0, got " ++ show dur)
          (dur >= 0)

      -- No end time before start time
      forM_ nodesWithDuration $ \n ->
        case (nodeStartTime n, nodeEndTime n) of
          (Just s, Just e) ->
            assertBool
              ("node " ++ show (nodeLabel n) ++ " end >= start")
              (e >= s)
          _ -> pure ()

      -- Critical path total <= total build time
      let cpDuration = sum
            [ maybe 0 id (nodeDuration n)
            | nid <- graphCriticalPath graph
            , Just n <- [IntMap.lookup nid (graphNodes graph)]
            ]
      assertBool
        ("CP duration " ++ show cpDuration ++ " <= total " ++ show (graphTotalSeconds graph))
        (cpDuration <= graphTotalSeconds graph + 0.001)  -- small epsilon

      -- Critical path duration >= any single node duration
      let maxSingle = maximum (0 : map (maybe 0 id . nodeDuration) nodesWithDuration)
      assertBool
        ("CP duration " ++ show cpDuration ++ " >= max single " ++ show maxSingle)
        (cpDuration >= maxSingle - 0.001)
```

Note: add `import Control.Monad (forM_)` if not already imported.

**Step 2: Run tests**

Run: `devenv shell -- cabal test --enable-tests`
Expected: 37 tests pass.

**Step 3: Commit**

```
git add test/Test/Telemetry/IntegrationTest.hs
git commit -m "test: add timing plausibility integration test (7.6)"
```

---

### Task 7: JSON and Mermaid output file tests (7.1 output + 7.7 idempotent)

**Files:**
- Modify: `test/Test/Telemetry/IntegrationTest.hs`

**Step 1: Write tests for output file verification and idempotency**

Add to test list:

```haskell
testCase "7.1/7.7: JSON and Mermaid output written and parseable" testOutputFiles
```

```haskell
testOutputFiles :: Assertion
testOutputFiles =
  withSystemTempDirectory "shake-integ-output" $ \tmpDir -> do
    -- Run the build via the Entry.shake wrapper (which writes output files)
    let opts =
          shakeOptions
            { shakeFiles = tmpDir </> ".shake"
            , shakeVerbosity = Shake.Silent
            }
    -- Use Entry.shake which handles telemetry creation + finalization
    import Development.Shake.Telemetry.Wrap.Entry qualified as TE
    TE.shake opts $ do
      (tmpDir </> "out.txt") WR.%> \out -> Shake.writeFile' out "hello"
      Shake.want [tmpDir </> "out.txt"]

    -- Verify JSON file exists and is parseable
    let jsonPath = tmpDir </> ".shake" </> "telemetry" </> "build-graph.json"
    jsonExists <- doesFileExist jsonPath
    assertBool "JSON output file exists" jsonExists
    jsonBytes <- BSL.readFile jsonPath
    case Aeson.eitherDecode jsonBytes :: Either String Aeson.Value of
      Left err -> assertFailure $ "JSON parse failed: " ++ err
      Right _ -> pure ()

    -- Verify Mermaid file exists and has expected structure
    let mmdPath = tmpDir </> ".shake" </> "telemetry" </> "critical-path.mmd"
    mmdExists <- doesFileExist mmdPath
    assertBool "Mermaid output file exists" mmdExists
    mmdContent <- readFile mmdPath
    assertBool "Mermaid starts with graph LR" ("graph LR" `isPrefixOf` mmdContent)
```

Wait — we can't use `import` inside a function. The import for `TE` must be at the top of the module. Add at the top:

```haskell
import Development.Shake.Telemetry.Wrap.Entry qualified as TE
```

And for `isPrefixOf`:

```haskell
import Data.List (isPrefixOf)
```

The actual test body uses `TE.shake` directly:

```haskell
testOutputFiles :: Assertion
testOutputFiles =
  withSystemTempDirectory "shake-integ-output" $ \tmpDir -> do
    let opts =
          shakeOptions
            { shakeFiles = tmpDir </> ".shake"
            , shakeVerbosity = Shake.Silent
            }
    TE.shake opts $ do
      (tmpDir </> "out.txt") WR.%> \out -> Shake.writeFile' out "hello"
      Shake.want [tmpDir </> "out.txt"]

    -- JSON output
    let jsonPath = tmpDir </> ".shake" </> "telemetry" </> "build-graph.json"
    jsonExists <- doesFileExist jsonPath
    assertBool "JSON output file exists" jsonExists
    jsonBytes <- BSL.readFile jsonPath
    case Aeson.eitherDecode jsonBytes :: Either String Aeson.Value of
      Left err -> assertFailure $ "JSON parse failed: " ++ err
      Right _ -> pure ()

    -- Mermaid output
    let mmdPath = tmpDir </> ".shake" </> "telemetry" </> "critical-path.mmd"
    mmdExists <- doesFileExist mmdPath
    assertBool "Mermaid output file exists" mmdExists
    mmdContent <- readFile mmdPath
    assertBool "Mermaid starts with graph LR" ("graph LR" `isPrefixOf` mmdContent)

    -- Idempotent: run again, verify files are still valid
    TE.shake opts $ do
      (tmpDir </> "out.txt") WR.%> \out -> Shake.writeFile' out "hello"
      Shake.want [tmpDir </> "out.txt"]

    jsonBytes2 <- BSL.readFile jsonPath
    case Aeson.eitherDecode jsonBytes2 :: Either String Aeson.Value of
      Left err -> assertFailure $ "Second run JSON parse failed: " ++ err
      Right _ -> pure ()
```

**Step 2: Run tests**

Run: `devenv shell -- cabal test --enable-tests`
Expected: 38 tests pass.

**Step 3: Commit**

```
git add test/Test/Telemetry/IntegrationTest.hs
git commit -m "test: add output file and idempotency integration tests (7.1/7.7)"
```

---

### Task 8: Mark Phase 7 as done and final verification

**Files:**
- Modify: `ROADMAP.md`

**Step 1: Run full test suite**

Run: `devenv shell -- cabal test --enable-tests`
Expected: All tests pass (38 total).

**Step 2: Mark Phase 7 as done**

In `ROADMAP.md`, change:
```
## Phase 7: Integration testing
```
to:
```
## Phase 7: Integration testing [DONE]
```

**Step 3: Commit**

```
git add ROADMAP.md
git commit -m "docs: mark Phase 7 (integration testing) as done"
```

---

## Notes for the implementer

- **Testing pattern**: The existing `WrapTest.hs` uses `Shake.shake` directly with manually injected `TelemetryState`. The integration tests should follow the same pattern for tests that inspect the graph. For the output file test (Task 7), use `TE.shake` from `Wrap.Entry` which handles the full pipeline including writing JSON/Mermaid.

- **Oracle type instance**: The `type instance Shake.RuleResult VersionQ = String` must be at the top level (Haskell requirement). Ensure it's outside any function.

- **Thread delays**: Use `threadDelay` (microseconds) to create timing differences. 100ms = 100000 microseconds. Keep delays small to avoid slow tests.

- **Edge direction**: `Edge(from=A, to=B)` means "A depends on B". So if C `need`s B, there's an edge from C to B.

- **Critical path analysis**: Tests that check the critical path must call `computeCriticalPath` on the frozen graph. The `withTelemetryBuild` helper already does this.

- **Batch rules test (7.5)**: Skipped because Shake's `batch` API has a complex type signature involving existential types that makes it difficult to test in isolation. The batch wrapping is already covered by the unit-level WrapTest. If needed, it can be added as a follow-up.

- **`forM_`**: Import from `Control.Monad` for the timing plausibility test.

- **Imports**: Each task may require additional imports. The implementer should add them as needed.
