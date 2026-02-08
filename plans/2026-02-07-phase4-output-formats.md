# Phase 4: Output Formats Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement JSON and Mermaid output so a `BuildGraph` (after critical path analysis) can be written to disk as machine-readable JSON and a human-readable Mermaid chart.

**Architecture:** Pure functions on `BuildGraph`. JSON uses aeson with custom `ToJSON`/`FromJSON` instances matching the schema in `ARCHITECTURE.md`. Mermaid renders only the critical path subgraph. Both have `write*` IO wrappers that create parent directories.

**Tech Stack:** `aeson` for JSON, `Data.Text` for Mermaid rendering, `directory` for `createDirectoryIfMissing`, `filepath` for `takeDirectory`.

---

### Task 1: JSON serialization — write failing tests

**Files:**
- Create: `test/Test/Telemetry/JsonTest.hs`
- Modify: `test/Main.hs` (add import + test group)
- Modify: `shake-telemetry.cabal` (add test module, add `aeson` + `bytestring` to test deps)

**Step 1: Write the test file**

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Test.Telemetry.JsonTest (jsonTests) where

import Data.Aeson qualified as Aeson
import Data.IntMap.Strict qualified as IntMap
import Data.Text (Text)
import Data.Time (UTCTime (..))
import Data.Time.Calendar (fromGregorian)
import Data.Vector qualified as Vector
import Test.Tasty
import Test.Tasty.HUnit

import Development.Shake.Telemetry.Graph
import Development.Shake.Telemetry.Json

-- Reuse the mkGraph helper pattern from CriticalPathTest
mkGraph :: [(Int, Text, Double)] -> [(Int, Int)] -> [Int] -> BuildGraph
mkGraph nodeSpecs edgeSpecs cp =
  BuildGraph
    { graphNodes = IntMap.fromList [(nid, mkNode nid lbl dur) | (nid, lbl, dur) <- nodeSpecs]
    , graphEdges = Vector.fromList [Edge from to | (from, to) <- edgeSpecs]
    , graphBuildStart = UTCTime (fromGregorian 2024 1 15) 37800
    , graphTotalSeconds = 120.5
    , graphCriticalPath = cp
    }
  where
    mkNode nid lbl dur = Node nid lbl FileNode (Just 0) (Just dur) (Just dur)

jsonTests :: TestTree
jsonTests = testGroup "Json"
  [ testCase "round-trip: encode then decode equals original" $ do
      let graph = mkGraph [(0,"a.o",1.0),(1,"b.o",2.0)] [(1,0)] [0,1]
          encoded = Aeson.encode graph
      case Aeson.eitherDecode encoded of
        Left err -> assertFailure ("decode failed: " ++ err)
        Right decoded -> assertEqual "round-trip" graph decoded
  , testCase "node with no timing data serializes with nulls" $ do
      let node = Node 0 "placeholder" FileNode Nothing Nothing Nothing
          graph = BuildGraph
            { graphNodes = IntMap.singleton 0 node
            , graphEdges = Vector.empty
            , graphBuildStart = UTCTime (fromGregorian 2024 1 1) 0
            , graphTotalSeconds = 0
            , graphCriticalPath = []
            }
          encoded = Aeson.encode graph
      case Aeson.eitherDecode encoded of
        Left err -> assertFailure ("decode failed: " ++ err)
        Right decoded -> assertEqual "round-trip with nulls" graph decoded
  , testCase "criticalPath field present in JSON" $ do
      let graph = mkGraph [(0,"x",1.0)] [] [0]
          val = Aeson.toJSON graph
      case val of
        Aeson.Object obj -> assertBool "has criticalPath key" ("criticalPath" `elem` fmap fst (Data.Aeson.KeyMap.toList obj))
        _ -> assertFailure "expected JSON object"
  , testCase "onCriticalPath set correctly on nodes" $ do
      let graph = mkGraph [(0,"a",1.0),(1,"b",2.0),(2,"c",3.0)] [(1,0),(2,1)] [0,1]
          val = Aeson.toJSON graph
      -- Node 0 and 1 are on CP, node 2 is not. Check via decode of the JSON structure.
      case Aeson.eitherDecode (Aeson.encode val) of
        Left err -> assertFailure err
        Right decoded -> assertEqual "round-trip" graph decoded
  ]
```

Note: The `criticalPath` field test and `onCriticalPath` test will need to be refined once we see the actual JSON structure. The key point is the tests exist and compile against the `Json` module's exports.

**Step 2: Add to Main.hs and cabal**

Add `import Test.Telemetry.JsonTest (jsonTests)` to `test/Main.hs` and `jsonTests` to the test group. Add `Test.Telemetry.JsonTest` to `other-modules` in cabal. Add `aeson` and `bytestring` to test `build-depends`.

**Step 3: Run tests — expect compile failure**

Run: `devenv shell -- cabal test --enable-tests`
Expected: Fails because `Development.Shake.Telemetry.Json` exports nothing yet.

**Step 4: Commit**

```
git add test/ shake-telemetry.cabal
git commit -m "Add failing JSON round-trip tests"
```

---

### Task 2: JSON serialization — implement

**Files:**
- Modify: `src/Development/Shake/Telemetry/Json.hs`
- Modify: `shake-telemetry.cabal` (add `directory` and `filepath` to lib deps)

**Step 1: Implement Json.hs**

The JSON schema from ARCHITECTURE.md:
- `NodeType` serializes as lowercase string: `"file"`, `"phony"`, `"oracle"`, `"batch"`, `"directory"`, `"env"`, `"action"`.
- `Node` serializes as an object with keys: `id`, `label`, `type`, `startTime`, `endTime`, `duration`, `onCriticalPath`. The `onCriticalPath` field is NOT stored in the `Node` Haskell type — it's computed during serialization from `graphCriticalPath`.
- `Edge` serializes as `{"from": N, "to": N}`.
- `BuildGraph` serializes as `{"buildStart": ..., "totalSeconds": ..., "nodes": [...], "edges": [...], "criticalPath": [...]}`.

For `FromJSON`, `onCriticalPath` on nodes should be ignored during decoding (it's derived data).

Since `onCriticalPath` is a derived field computed during serialization, the `ToJSON Node` instance alone can't produce it — it needs the critical path list. So we need a custom `ToJSON BuildGraph` that enriches nodes with the `onCriticalPath` flag.

Approach:
- Write `ToJSON`/`FromJSON` for `NodeType` manually (string mapping).
- Write `ToJSON`/`FromJSON` for `Edge` (simple object).
- Write custom `ToJSON BuildGraph` that embeds `onCriticalPath` into each node's JSON.
- Write `FromJSON BuildGraph` that ignores `onCriticalPath` on nodes.
- `writeJsonGraph :: FilePath -> BuildGraph -> IO ()` — encode + write, creating parent dirs.

Add `directory` and `filepath` to lib build-depends in cabal.

**Step 2: Run tests**

Run: `devenv shell -- cabal test --enable-tests`
Expected: All JSON tests pass. Fix the `criticalPath` field test if needed (it was speculative).

**Step 3: Commit**

```
git add src/Development/Shake/Telemetry/Json.hs shake-telemetry.cabal
git commit -m "Implement JSON serialization with aeson"
```

**Step 4: Fix any test issues and commit**

If the speculative tests need adjustments (e.g., the KeyMap import for checking keys), fix them and commit:

```
git add test/
git commit -m "Fix JSON test assertions"
```

---

### Task 3: Mermaid generation — write failing tests

**Files:**
- Create: `test/Test/Telemetry/MermaidTest.hs`
- Modify: `test/Main.hs` (add import + test group)
- Modify: `shake-telemetry.cabal` (add test module)

**Step 1: Write the test file**

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Test.Telemetry.MermaidTest (mermaidTests) where

import Data.IntMap.Strict qualified as IntMap
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime (..))
import Data.Time.Calendar (fromGregorian)
import Data.Vector qualified as Vector
import Test.Tasty
import Test.Tasty.HUnit

import Development.Shake.Telemetry.Graph
import Development.Shake.Telemetry.Mermaid

mkGraph :: [(Int, Text, Double)] -> [(Int, Int)] -> [Int] -> BuildGraph
mkGraph nodeSpecs edgeSpecs cp = ...  -- same helper

mermaidTests :: TestTree
mermaidTests = testGroup "Mermaid"
  [ testCase "renders critical path as graph LR" $ do
      let graph = mkGraph [(0,"compile",2.5),(1,"link",5.0)] [(1,0)] [0,1]
          output = renderMermaid graph
      assertBool "starts with graph LR" ("graph LR" `T.isPrefixOf` output)
      assertBool "has classDef" ("classDef critical" `T.isInfixOf` output)
      assertBool "has edge arrow" ("-->" `T.isInfixOf` output)
      assertBool "has class statement" ("class " `T.isInfixOf` output)
  , testCase "single node critical path" $ do
      let graph = mkGraph [(0,"only",3.0)] [] [0]
          output = renderMermaid graph
      assertBool "has node label" ("only" `T.isInfixOf` output)
      assertBool "has duration" ("3.0s" `T.isInfixOf` output)
  , testCase "empty critical path produces minimal output" $ do
      let graph = mkGraph [(0,"a",1.0)] [] []
          output = renderMermaid graph
      assertBool "starts with graph LR" ("graph LR" `T.isPrefixOf` output)
  ]
```

**Step 2: Wire into Main.hs and cabal**

**Step 3: Run tests — expect compile failure**

**Step 4: Commit**

```
git add test/ shake-telemetry.cabal
git commit -m "Add failing Mermaid snapshot tests"
```

---

### Task 4: Mermaid generation — implement

**Files:**
- Modify: `src/Development/Shake/Telemetry/Mermaid.hs`

**Step 1: Implement Mermaid.hs**

`renderMermaid :: BuildGraph -> Text` — renders only the critical path:
- Header: `graph LR`
- ClassDef: `classDef critical fill:#ff6b6b,stroke:#333,stroke-width:2px`
- For each consecutive pair in `graphCriticalPath`, emit: `nX["label (Ds)"] --> nY["label (Ds)"]`
- Single-node critical path: just emit the node, no edges.
- Footer: `class nX,nY,... critical`

`writeMermaidChart :: FilePath -> BuildGraph -> IO ()` — render + write, creating parent dirs.

**Step 2: Run tests**

Run: `devenv shell -- cabal test --enable-tests`
Expected: All Mermaid tests pass.

**Step 3: Commit**

```
git add src/Development/Shake/Telemetry/Mermaid.hs
git commit -m "Implement Mermaid critical path rendering"
```

---

### Task 5: Final verification and phase completion

**Step 1: Run full test suite**

Run: `devenv shell -- cabal test --enable-tests`
Expected: All tests pass (20 existing + new JSON + Mermaid tests).

**Step 2: Mark phase done**

Edit `ROADMAP.md`: change `## Phase 4: Output formats` to `## Phase 4: Output formats [DONE]`.

**Step 3: Commit**

```
git add ROADMAP.md
git commit -m "Mark Phase 4 as done"
```
