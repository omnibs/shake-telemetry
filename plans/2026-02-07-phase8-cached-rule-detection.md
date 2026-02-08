# Phase 8: Cached Rule Detection Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Ensure cached (skipped) rules appear in the telemetry graph with `nodeDuration = Nothing`, so users can distinguish executed rules from cache hits.

**Architecture:** On a partial rebuild, when rule A executes and calls `T.need ["b.txt"]`, `recordEdge` already calls `getOrCreateNode` which creates B as a placeholder node with `Nothing` timing. So direct dependencies of executing rules already appear as cached placeholders. The main work is: (1) verifying and testing this existing behavior, (2) marking the `NodeType` correctly on these placeholders, and (3) documenting the limitation that transitive dependencies through cached rules and fully-cached builds produce empty/incomplete graphs (since Shake provides no per-rule callback for skipped rules).

**Tech Stack:** Haskell, GHC2021, tasty + tasty-hunit, Development.Shake.Telemetry.*

---

### Task 1: Write test for partial rebuild — cached direct dependency

This test verifies that when rule A executes and `need`s B, and B is cached (didn't re-execute), B still appears in the graph as a placeholder node with `nodeDuration = Nothing`.

**Files:**
- Modify: `test/Test/Telemetry/IntegrationTest.hs`

**Step 1: Write the failing test**

Add to the `integrationTests` test list:

```haskell
testCase "8.3: cached rules appear with no timing" testCachedRules
```

Add the test function. The test runs two builds:
- Build 1: clean build of chain C -> B -> A (C needs B, B needs A).
- Build 2: touch only A's source, so A and B re-execute but C is cached (if C doesn't depend on the content that changed). Actually, Shake's caching is content-based — if B's output changes, C will rebuild too. We need a setup where C's dependency is cached.

Simpler approach: two independent rules, X and Y. X needs Y. Build 1: both execute. Build 2: modify X's source so X re-executes and calls `T.need [Y]`, but Y is cached. Y should appear as a placeholder.

Actually even simpler: build 1 builds everything. Build 2 only wants X (which needs Y). If Y hasn't changed, Y is cached. X re-executes because... hmm, Shake will also cache X if nothing changed.

The cleanest approach: use `alwaysRerun` in one rule to force it to re-execute, while its dependency is cached.

```haskell
testCachedRules :: Assertion
testCachedRules =
  withSystemTempDirectory "shake-integ-cached" $ \tmpDir -> do
    let shakeDir = tmpDir </> ".shake"

    -- Build 1: clean build, both rules execute
    state1 <- newTelemetryState
    let opts1 = shakeOptions
          { shakeFiles = shakeDir
          , shakeVerbosity = T.Silent
          , shakeExtra = T.addShakeExtra state1 (shakeExtra shakeOptions)
          }
    Shake.shake opts1 $ do
      (tmpDir </> "dep.txt") T.%> \out ->
        T.writeFile' out "dep-content"
      (tmpDir </> "main.txt") T.%> \out -> do
        T.need [tmpDir </> "dep.txt"]
        T.writeFile' out "main-content"
      T.want [tmpDir </> "main.txt"]

    graph1 <- freezeGraph state1
    let analyzed1 = computeCriticalPath graph1

    -- Both nodes should have timing data after clean build
    let depNode1 = findNodeByLabel "dep.txt" analyzed1
    let mainNode1 = findNodeByLabel "main.txt" analyzed1
    assertBool "clean build: dep.txt exists" (isJust depNode1)
    assertBool "clean build: main.txt exists" (isJust mainNode1)
    assertBool "clean build: dep.txt has duration"
      (isJust (depNode1 >>= nodeDuration))
    assertBool "clean build: main.txt has duration"
      (isJust (mainNode1 >>= nodeDuration))

    -- Build 2: same rules, but dep.txt is cached. Force main.txt to
    -- re-execute by using alwaysRerun.
    state2 <- newTelemetryState
    let opts2 = shakeOptions
          { shakeFiles = shakeDir
          , shakeVerbosity = T.Silent
          , shakeExtra = T.addShakeExtra state2 (shakeExtra shakeOptions)
          }
    Shake.shake opts2 $ do
      (tmpDir </> "dep.txt") T.%> \out ->
        T.writeFile' out "dep-content"
      (tmpDir </> "main.txt") T.%> \out -> do
        T.alwaysRerun
        T.need [tmpDir </> "dep.txt"]
        T.writeFile' out "main-content"
      T.want [tmpDir </> "main.txt"]

    graph2 <- freezeGraph state2
    let analyzed2 = computeCriticalPath graph2

    -- main.txt should have timing data (it re-executed due to alwaysRerun)
    let mainNode2 = findNodeByLabel "main.txt" analyzed2
    assertBool "incremental: main.txt exists" (isJust mainNode2)
    assertBool "incremental: main.txt has duration"
      (isJust (mainNode2 >>= nodeDuration))

    -- dep.txt should exist as a placeholder with NO timing data (cached)
    let depNode2 = findNodeByLabel "dep.txt" analyzed2
    assertBool "incremental: dep.txt exists (as placeholder)" (isJust depNode2)
    assertEqual "incremental: dep.txt has no duration (cached)"
      Nothing (depNode2 >>= nodeDuration)

    -- Edge from main.txt to dep.txt should still exist
    case (mainNode2, depNode2) of
      (Just mn, Just dn) -> do
        let hasEdge = Vector.any
              (\e -> edgeFrom e == nodeId mn && edgeTo e == nodeId dn)
              (graphEdges analyzed2)
        assertBool "incremental: edge from main.txt to dep.txt exists" hasEdge
      _ -> assertFailure "missing nodes for edge check"
```

Note: you'll need to add `import Data.Maybe (isJust)` at the top of the file.

**Step 2: Run test to verify it works (or fails)**

Run: `devenv shell -- cabal test --enable-tests`

This test should PASS with the existing code because `recordEdge` → `getOrCreateNode` already creates placeholder nodes. If it fails, we need to investigate why.

**Step 3: Commit**

```
git add test/Test/Telemetry/IntegrationTest.hs
git commit -m "test: add cached rule detection test (8.3)"
```

---

### Task 2: Write test for mixed cached and rebuilt rules

**Files:**
- Modify: `test/Test/Telemetry/IntegrationTest.hs`

**Step 1: Write the test**

Add to the `integrationTests` test list:

```haskell
testCase "8.4: mixed cached and rebuilt" testMixedCachedRebuilt
```

This test has three rules: A (leaf, cached), B (depends on A, cached), C (depends on B, forced to re-execute). C calls `T.need [B]`, so B appears as a placeholder. But B's body doesn't run, so B's `T.need [A]` never fires, and A is absent from the graph.

```haskell
testMixedCachedRebuilt :: Assertion
testMixedCachedRebuilt =
  withSystemTempDirectory "shake-integ-mixed" $ \tmpDir -> do
    let shakeDir = tmpDir </> ".shake"
        mkOpts st = shakeOptions
          { shakeFiles = shakeDir
          , shakeVerbosity = T.Silent
          , shakeExtra = T.addShakeExtra st (shakeExtra shakeOptions)
          }
        rules = do
          (tmpDir </> "a.txt") T.%> \out ->
            T.writeFile' out "a"
          (tmpDir </> "b.txt") T.%> \out -> do
            T.need [tmpDir </> "a.txt"]
            T.writeFile' out "b"
          (tmpDir </> "c.txt") T.%> \out -> do
            T.alwaysRerun
            T.need [tmpDir </> "b.txt"]
            T.writeFile' out "c"
          T.want [tmpDir </> "c.txt"]

    -- Build 1: clean
    state1 <- newTelemetryState
    Shake.shake (mkOpts state1) rules
    graph1 <- freezeGraph state1
    let analyzed1 = computeCriticalPath graph1

    -- All 3 should have timing data
    forM_ ["a.txt", "b.txt", "c.txt"] $ \name -> do
      let mNode = findNodeByLabel name analyzed1
      assertBool ("clean: " ++ Text.unpack name ++ " has duration")
        (isJust (mNode >>= nodeDuration))

    -- Build 2: incremental (only c.txt re-executes due to alwaysRerun)
    state2 <- newTelemetryState
    Shake.shake (mkOpts state2) rules
    graph2 <- freezeGraph state2
    let analyzed2 = computeCriticalPath graph2

    -- c.txt re-executed: has timing data
    let cNode = findNodeByLabel "c.txt" analyzed2
    assertBool "incremental: c.txt has duration"
      (isJust (cNode >>= nodeDuration))

    -- b.txt is a direct dependency of c.txt: exists as placeholder, no timing
    let bNode = findNodeByLabel "b.txt" analyzed2
    assertBool "incremental: b.txt exists (placeholder)" (isJust bNode)
    assertEqual "incremental: b.txt has no duration (cached)"
      Nothing (bNode >>= nodeDuration)

    -- a.txt is a transitive dependency through cached b.txt: NOT in graph
    -- (b.txt's body didn't run, so its need [a.txt] never fired)
    let aNode = findNodeByLabel "a.txt" analyzed2
    assertBool "incremental: a.txt absent (transitive through cached)"
      (not (isJust aNode))
```

**Step 2: Run tests**

Run: `devenv shell -- cabal test --enable-tests`

This should PASS with the existing code. If it does, it confirms:
- Direct cached dependencies appear as placeholders (no timing).
- Transitive dependencies through cached rules are absent (documented limitation).

**Step 3: Commit**

```
git add test/Test/Telemetry/IntegrationTest.hs
git commit -m "test: add mixed cached/rebuilt test (8.4)"
```

---

### Task 3: Write test for fully cached build

**Files:**
- Modify: `test/Test/Telemetry/IntegrationTest.hs`

**Step 1: Write the test**

Add to the `integrationTests` test list:

```haskell
testCase "8.3b: fully cached build produces empty graph" testFullyCached
```

```haskell
testFullyCached :: Assertion
testFullyCached =
  withSystemTempDirectory "shake-integ-fullcache" $ \tmpDir -> do
    let shakeDir = tmpDir </> ".shake"
        mkOpts st = shakeOptions
          { shakeFiles = shakeDir
          , shakeVerbosity = T.Silent
          , shakeExtra = T.addShakeExtra st (shakeExtra shakeOptions)
          }
        rules = do
          (tmpDir </> "out.txt") T.%> \out ->
            T.writeFile' out "content"
          T.want [tmpDir </> "out.txt"]

    -- Build 1: clean
    state1 <- newTelemetryState
    Shake.shake (mkOpts state1) rules

    -- Build 2: nothing changed, everything cached
    state2 <- newTelemetryState
    Shake.shake (mkOpts state2) rules
    graph2 <- freezeGraph state2

    -- Graph should be empty — no rules executed, no need calls fired
    let nodeCount = IntMap.size (graphNodes graph2)
    assertEqual "fully cached build: no nodes" 0 nodeCount
```

**Step 2: Run tests**

Run: `devenv shell -- cabal test --enable-tests`

This documents the current behavior: a fully cached build produces an empty graph.

**Step 3: Commit**

```
git add test/Test/Telemetry/IntegrationTest.hs
git commit -m "test: add fully cached build test (8.3b) documenting empty graph"
```

---

### Task 4: Mark Phase 8 as done and update ROADMAP

**Files:**
- Modify: `ROADMAP.md`

**Step 1: Run full test suite**

Run: `devenv shell -- cabal test --enable-tests`
Expected: All tests pass.

**Step 2: Update ROADMAP**

Change `## Phase 8: Cached rule detection` to `## Phase 8: Cached rule detection [DONE]`.

Add a note at the end of Phase 8 documenting the known limitations:

```markdown
### Known limitations

- **Transitive dependencies through cached rules are absent.** If rule C needs B and B needs A, and B is cached, only C and B appear in the graph. A is absent because B's body (containing `need [A]`) never executes.
- **Fully cached builds produce empty graphs.** If nothing re-executes, no telemetry is recorded. The previous build's output files remain available.
- **Shake does not expose per-rule skip callbacks.** The `shakeProgress` API provides aggregate `countSkipped`/`countBuilt` counts but not per-rule information. Solving the above limitations would require Shake upstream changes or database introspection.
```

**Step 3: Commit**

```
git add ROADMAP.md
git commit -m "docs: mark Phase 8 done, document cached rule limitations"
```

---

## Notes for the implementer

- **`alwaysRerun`** is re-exported from `Development.Shake.Telemetry` (via Reexports). It forces a rule to re-execute every build, which is how we create the "some cached, some rebuilt" scenario in tests.

- **`Shake.shake` vs `T.shake`**: The tests use `Shake.shake` (raw, unwrapped) with manually injected state, same as the existing integration tests. This is because `T.shake` creates its own state. See the comment on `runBuild` in IntegrationTest.hs.

- **Edge direction**: `Edge(from=A, to=B)` means "A depends on B". When C calls `T.need [B]`, an edge from C to B is recorded.

- **Placeholder nodes**: `getOrCreateNode` in `State.hs` already creates nodes with `Nothing` timing when called from `recordEdge`. These are the "cached rule" indicators — nodes with no `startTime`, `endTime`, or `duration`.

- **`isJust`**: Import from `Data.Maybe`.

- **`Text.unpack`**: `Data.Text` is imported as `Text` in IntegrationTest.hs.

- **No implementation code changes expected.** The existing `getOrCreateNode`/`recordEdge` mechanism already produces the right behavior. This phase is primarily about testing and documenting.
