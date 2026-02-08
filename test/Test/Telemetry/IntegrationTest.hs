{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.Telemetry.IntegrationTest (integrationTests) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.IntMap.Strict qualified as IntMap
import Data.List (isPrefixOf, sort)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Development.Shake qualified as Shake (shake)
import Development.Shake.Telemetry qualified as T
import Development.Shake.Telemetry (Binary, Hashable, NFData, ShakeOptions (..), Typeable, shakeOptions)
import Development.Shake.Telemetry.CriticalPath (computeCriticalPath)
import Development.Shake.Telemetry.Graph
import Development.Shake.Telemetry.State (freezeGraph, newTelemetryState)
import GHC.Generics (Generic)
import System.Directory (doesFileExist, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

-- Oracle key type for test 7.3
newtype VersionQ = VersionQ () deriving (Eq, Show, Typeable, Generic)
instance Hashable VersionQ
instance Binary VersionQ
instance NFData VersionQ
type instance T.RuleResult VersionQ = String

-- | Run a Shake build with telemetry, returning the analyzed graph.
withTelemetryBuild
  :: String
  -> (FilePath -> ShakeOptions -> IO ())
  -> (FilePath -> BuildGraph -> Assertion)
  -> Assertion
withTelemetryBuild name buildFn checkFn =
  withSystemTempDirectory ("shake-integ-" ++ name) $ \tmpDir -> do
    state <- newTelemetryState
    let opts =
          shakeOptions
            { shakeFiles = tmpDir </> ".shake"
            , shakeVerbosity = T.Silent
            , shakeExtra = T.addShakeExtra state (shakeExtra shakeOptions)
            }
    buildFn tmpDir opts
    graph <- freezeGraph state
    let analyzed = computeCriticalPath graph
    checkFn tmpDir analyzed

-- | Run a Shake build using the raw (unwrapped) Shake.shake entry point.
-- We can't use T.shake here because it creates its own TelemetryState and
-- injects it into shakeExtra, overwriting the one that withTelemetryBuild
-- already injected. That would leave withTelemetryBuild freezing an empty
-- state while the real telemetry data lives in T.shake's private state.
runBuild :: ShakeOptions -> T.Rules () -> IO ()
runBuild = Shake.shake

-- | Find all nodes of a given type.
nodesOfType :: NodeType -> BuildGraph -> [Node]
nodesOfType ntype graph =
  filter (\n -> nodeType n == ntype) (IntMap.elems (graphNodes graph))

-- | Find a node by label substring.
findNodeByLabel :: Text -> BuildGraph -> Maybe Node
findNodeByLabel substr graph =
  let nodes = IntMap.elems (graphNodes graph)
  in case filter (\n -> substr `Text.isInfixOf` nodeLabel n) nodes of
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

-- | Read a file strictly as a ByteString (closes handle immediately).
readFileStrict :: FilePath -> IO BS.ByteString
readFileStrict = BS.readFile

-- | Read a file strictly as a String (closes handle immediately).
readFileStrictText :: FilePath -> IO String
readFileStrictText path = do
  bs <- BS.readFile path
  pure (map (toEnum . fromEnum) (BS.unpack bs))

integrationTests :: TestTree
integrationTests =
  testGroup
    "Integration"
    [ testCase "7.1: linear chain A -> B -> C" testLinearChain
    , testCase "7.2: diamond with slow branch" testDiamond
    , testCase "7.3: oracle build" testOracle
    , testCase "7.4: parallel preserves edges" testParallel
    , testCase "7.6: timing plausibility" testTimingPlausibility
    , testCase "7.1/7.7: output files and idempotency" testOutputFiles
    ]

-- | Test 7.1: Linear chain A -> B -> C
-- a.txt is a leaf, b.txt needs a.txt, c.txt needs b.txt.
testLinearChain :: Assertion
testLinearChain =
  withTelemetryBuild "linear" buildFn checkFn
  where
    buildFn tmpDir opts = runBuild opts $ do
      (tmpDir </> "a.txt") T.%> \out ->
        T.writeFile' out "a-content"
      (tmpDir </> "b.txt") T.%> \out -> do
        T.need [tmpDir </> "a.txt"]
        T.writeFile' out "b-content"
      (tmpDir </> "c.txt") T.%> \out -> do
        T.need [tmpDir </> "b.txt"]
        T.writeFile' out "c-content"
      T.want [tmpDir </> "c.txt"]

    checkFn _tmpDir graph = do
      let fileNodes = nodesOfType FileNode graph
      assertEqual "expected 3 FileNodes" 3 (length fileNodes)
      let edgeCount = Vector.length (graphEdges graph)
      assertBool
        ("expected >= 2 edges, got " ++ show edgeCount)
        (edgeCount >= 2)
      let cpLen = length (graphCriticalPath graph)
      assertEqual "critical path has 3 nodes" 3 cpLen

-- | Test 7.2: Diamond dependency with slow branch
-- a.txt (leaf), b.txt needs a.txt + 100ms delay, c.txt needs a.txt (fast),
-- d.txt needs b.txt and c.txt. Critical path should go through b.txt.
testDiamond :: Assertion
testDiamond =
  withTelemetryBuild "diamond" buildFn checkFn
  where
    buildFn tmpDir opts = runBuild opts $ do
      (tmpDir </> "a.txt") T.%> \out ->
        T.writeFile' out "a-content"
      (tmpDir </> "b.txt") T.%> \out -> do
        T.need [tmpDir </> "a.txt"]
        liftIO $ threadDelay 100000
        T.writeFile' out "b-content"
      (tmpDir </> "c.txt") T.%> \out -> do
        T.need [tmpDir </> "a.txt"]
        T.writeFile' out "c-content"
      (tmpDir </> "d.txt") T.%> \out -> do
        T.need [tmpDir </> "b.txt", tmpDir </> "c.txt"]
        T.writeFile' out "d-content"
      T.want [tmpDir </> "d.txt"]

    checkFn _tmpDir graph = do
      let fileNodes = nodesOfType FileNode graph
      assertEqual "expected 4 FileNodes" 4 (length fileNodes)
      let edgeCount = Vector.length (graphEdges graph)
      assertBool
        ("expected >= 4 edges, got " ++ show edgeCount)
        (edgeCount >= 4)
      -- Critical path should go through the slow branch (b.txt)
      let cpNodeIds = graphCriticalPath graph
          cpLabels = map (\nid -> maybe "" nodeLabel (IntMap.lookup nid (graphNodes graph))) cpNodeIds
          hasBtxt = any (Text.isInfixOf "b.txt") cpLabels
      assertBool
        ("critical path should include b.txt, labels: " ++ show cpLabels)
        hasBtxt

-- | Test 7.3: Oracle build
-- addOracle for VersionQ returning "1.0.0", file rule for version.txt
-- that calls askOracle (VersionQ ()). Should see an OracleNode.
testOracle :: Assertion
testOracle =
  withTelemetryBuild "oracle" buildFn checkFn
  where
    buildFn tmpDir opts = runBuild opts $ do
      _ <- T.addOracle $ \(VersionQ ()) -> pure "1.0.0"
      (tmpDir </> "version.txt") T.%> \out -> do
        ver <- T.askOracle (VersionQ ())
        T.writeFile' out ver
      T.want [tmpDir </> "version.txt"]

    checkFn _tmpDir graph = do
      let oracleNodes = nodesOfType OracleNode graph
      assertBool
        ("expected at least one OracleNode, got " ++ show (length oracleNodes))
        (not (null oracleNodes))
      -- The version.txt file node should have outgoing edges (to the oracle)
      let mVersionNode = findNodeByLabel "version.txt" graph
      case mVersionNode of
        Nothing -> assertFailure "version.txt node not found"
        Just vNode -> do
          let outEdges = edgesFrom (nodeId vNode) graph
          assertBool
            ("version.txt should have outgoing edges, got " ++ show outEdges)
            (outEdges > 0)

-- | Test 7.4: Parallel preserves edges
-- *.dep rule writes content, main.txt uses T.parallel to need x.dep, y.dep, z.dep.
testParallel :: Assertion
testParallel =
  withTelemetryBuild "parallel" buildFn checkFn
  where
    buildFn tmpDir opts = runBuild opts $ do
      (tmpDir </> "x.dep") T.%> \out -> T.writeFile' out "x"
      (tmpDir </> "y.dep") T.%> \out -> T.writeFile' out "y"
      (tmpDir </> "z.dep") T.%> \out -> T.writeFile' out "z"
      (tmpDir </> "main.txt") T.%> \out -> do
        _ <- T.parallel
          [ T.need [tmpDir </> "x.dep"]
          , T.need [tmpDir </> "y.dep"]
          , T.need [tmpDir </> "z.dep"]
          ]
        T.writeFile' out "main"
      T.want [tmpDir </> "main.txt"]

    checkFn tmpDir graph = do
      -- main.txt should have >= 3 outgoing edges (to x.dep, y.dep, z.dep)
      let mMainNode = findNodeByLabel "main.txt" graph
      case mMainNode of
        Nothing -> assertFailure "main.txt node not found"
        Just mainNode -> do
          let outEdges = edgesFrom (nodeId mainNode) graph
          assertBool
            ("main.txt should have >= 3 edges, got " ++ show outEdges)
            (outEdges >= 3)
      -- All 3 dep nodes should exist
      forM_ ["x.dep", "y.dep", "z.dep"] $ \dep ->
        assertBool (Text.unpack dep ++ " node should exist") $
          case findNodeByLabel dep graph of
            Just _ -> True
            Nothing -> False

-- | Test 7.6: Timing plausibility
-- slow.txt with 50ms delay, fast.txt needs slow.txt.
-- Check: durations >= 0, end >= start, CP duration within bounds.
testTimingPlausibility :: Assertion
testTimingPlausibility =
  withTelemetryBuild "timing" buildFn checkFn
  where
    buildFn tmpDir opts = runBuild opts $ do
      (tmpDir </> "slow.txt") T.%> \out -> do
        liftIO $ threadDelay 50000
        T.writeFile' out "slow"
      (tmpDir </> "fast.txt") T.%> \out -> do
        T.need [tmpDir </> "slow.txt"]
        T.writeFile' out "fast"
      T.want [tmpDir </> "fast.txt"]

    checkFn _tmpDir graph = do
      let nodes = IntMap.elems (graphNodes graph)
      -- All durations should be >= 0
      forM_ nodes $ \n ->
        case nodeDuration n of
          Just d -> assertBool
            ("duration should be >= 0 for " ++ Text.unpack (nodeLabel n) ++ ", got " ++ show d)
            (d >= 0)
          Nothing -> pure ()
      -- All end times should be >= start times
      forM_ nodes $ \n ->
        case (nodeStartTime n, nodeEndTime n) of
          (Just s, Just e) -> assertBool
            ("end >= start for " ++ Text.unpack (nodeLabel n) ++ ", start=" ++ show s ++ " end=" ++ show e)
            (e >= s)
          _ -> pure ()
      -- CP duration should be <= total build time + epsilon
      let epsilon = 0.05  -- 50ms tolerance
          cpNodeIds = graphCriticalPath graph
          cpDuration = sum [ maybe 0 id (nodeDuration n)
                           | nid <- cpNodeIds
                           , Just n <- [IntMap.lookup nid (graphNodes graph)]
                           ]
          totalTime = graphTotalSeconds graph
      assertBool
        ("CP duration (" ++ show cpDuration ++ ") should be <= total time (" ++ show totalTime ++ ") + epsilon")
        (cpDuration <= totalTime + epsilon)
      -- CP duration should be >= max single node duration - epsilon
      let allDurations = [ d | n <- nodes, Just d <- [nodeDuration n] ]
          maxDur = if null allDurations then 0 else maximum allDurations
      assertBool
        ("CP duration (" ++ show cpDuration ++ ") should be >= max node duration (" ++ show maxDur ++ ") - epsilon")
        (cpDuration >= maxDur - epsilon)

-- | Test 7.1/7.7: Output files and idempotency
-- Use T.shake to write output files, verify JSON and Mermaid, run again for idempotency.
-- Verify both runs produce same graph structure (same nodes and edges).
testOutputFiles :: Assertion
testOutputFiles =
  withSystemTempDirectory "shake-integ-output" $ \tmpDir -> do
    let shakeDir = tmpDir </> ".shake"
        opts = shakeOptions
          { shakeFiles = shakeDir
          , shakeVerbosity = T.Silent
          }
        jsonPath = shakeDir </> "telemetry" </> "build-graph.json"
        mmdPath = shakeDir </> "telemetry" </> "critical-path.mmd"
        rules = do
          (tmpDir </> "out.txt") T.%> \out ->
            T.writeFile' out "output"
          T.want [tmpDir </> "out.txt"]

    -- First build
    T.shake opts rules
    -- Check JSON file
    jsonExists <- doesFileExist jsonPath
    assertBool "build-graph.json should exist" jsonExists
    jsonBytes1 <- readFileStrict jsonPath
    json1 <- case Aeson.decode (BSL.fromStrict jsonBytes1) :: Maybe Aeson.Value of
      Nothing -> assertFailure "build-graph.json should be valid JSON" >> error "unreachable"
      Just v -> pure v
    -- Check Mermaid file
    mmdExists <- doesFileExist mmdPath
    assertBool "critical-path.mmd should exist" mmdExists
    mmdContent1 <- readFileStrictText mmdPath
    assertBool
      ("Mermaid file should start with 'graph LR', got: " ++ take 40 mmdContent1)
      ("graph LR" `isPrefixOf` mmdContent1)

    -- Idempotency: clean shake database and run the same build again
    removeDirectoryRecursive shakeDir
    T.shake opts rules
    -- Verify JSON still valid
    jsonBytes2 <- readFileStrict jsonPath
    json2 <- case Aeson.decode (BSL.fromStrict jsonBytes2) :: Maybe Aeson.Value of
      Nothing -> assertFailure "build-graph.json should still be valid JSON after second build" >> error "unreachable"
      Just v -> pure v
    -- Verify Mermaid still valid
    mmdContent2 <- readFileStrictText mmdPath
    assertBool
      ("Mermaid file should start with 'graph LR' after second build")
      ("graph LR" `isPrefixOf` mmdContent2)
    -- Verify structural equivalence: same node labels and edge count
    let extractStructure :: Aeson.Value -> Maybe ([String], Int)
        extractStructure (Aeson.Object obj) = do
          nodesVal <- KM.lookup "nodes" obj
          edgesVal <- KM.lookup "edges" obj
          nodeLabels <- case nodesVal of
            Aeson.Array arr -> Just
              [ Text.unpack lbl
              | Aeson.Object nObj <- Vector.toList arr
              , Just (Aeson.String lbl) <- [KM.lookup "label" nObj]
              ]
            _ -> Nothing
          edgeCount <- case edgesVal of
            Aeson.Array arr -> Just (Vector.length arr)
            _ -> Nothing
          Just (sort nodeLabels, edgeCount)
        extractStructure _ = Nothing
    case (extractStructure json1, extractStructure json2) of
      (Just (labels1, edges1), Just (labels2, edges2)) -> do
        assertEqual "same node labels across runs" labels1 labels2
        assertEqual "same edge count across runs" edges1 edges2
      _ -> assertFailure "could not extract structure from JSON"
