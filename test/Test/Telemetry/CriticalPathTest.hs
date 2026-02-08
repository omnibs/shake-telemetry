{-# LANGUAGE OverloadedStrings #-}

module Test.Telemetry.CriticalPathTest (criticalPathTests) where

import Data.IntMap.Strict qualified as IntMap
import Data.Text (Text)
import Data.Time (UTCTime (..))
import Data.Time.Calendar (fromGregorian)
import Data.Vector qualified as Vector
import Test.Tasty
import Test.Tasty.HUnit

import Development.Shake.Telemetry.CriticalPath
import Development.Shake.Telemetry.Graph

-- | Helper: build a graph from a list of (id, label, duration) and edges.
mkGraph :: [(Int, Text, Double)] -> [(Int, Int)] -> BuildGraph
mkGraph nodeSpecs edgeSpecs =
  BuildGraph
    { graphNodes = IntMap.fromList [(nid, mkNode nid lbl dur) | (nid, lbl, dur) <- nodeSpecs]
    , graphEdges = Vector.fromList [Edge from to | (from, to) <- edgeSpecs]
    , graphBuildStart = UTCTime (fromGregorian 2024 1 1) 0
    , graphTotalSeconds = 0
    , graphCriticalPath = []
    }
  where
    mkNode nid lbl dur =
      Node
        { nodeId = nid
        , nodeLabel = lbl
        , nodeType = FileNode
        , nodeStartTime = Just 0
        , nodeEndTime = Just dur
        , nodeDuration = Just dur
        }

-- | Sum of durations along the critical path.
criticalPathDuration :: BuildGraph -> Double
criticalPathDuration graph =
  sum [maybe 0 id (nodeDuration (graphNodes graph IntMap.! nid)) | nid <- graphCriticalPath graph]

criticalPathTests :: TestTree
criticalPathTests =
  testGroup
    "CriticalPath"
    [ testCase "linear chain: A -> B -> C" $ do
        -- A(1) -> B(2) -> C(3), edges: A depends on B, B depends on C
        -- But in our model edges go from dependent to dependency:
        -- C needs nothing, B needs C, A needs B
        -- Critical path: C -> B -> A = 3 + 2 + 1 = 6
        let graph =
              mkGraph
                [(0, "C", 3), (1, "B", 2), (2, "A", 1)]
                [(2, 1), (1, 0)] -- A->B, B->C
            result = computeCriticalPath graph
        assertEqual "critical path length" 3 (length (graphCriticalPath result))
        assertBool "total duration is 6" (abs (criticalPathDuration result - 6.0) < 0.001)
        -- Path should be C, B, A (sources first)
        assertEqual "path" [0, 1, 2] (graphCriticalPath result)
    , testCase "diamond: A -> {B, C} -> D" $ do
        -- D needs B and C, B needs A, C needs A
        -- Durations: A=1, B=5, C=2, D=1
        -- Critical path: A -> B -> D = 1 + 5 + 1 = 7
        let graph =
              mkGraph
                [(0, "A", 1), (1, "B", 5), (2, "C", 2), (3, "D", 1)]
                [(1, 0), (2, 0), (3, 1), (3, 2)] -- B->A, C->A, D->B, D->C
            result = computeCriticalPath graph
        assertEqual "path" [0, 1, 3] (graphCriticalPath result)
        assertBool "total duration is 7" (abs (criticalPathDuration result - 7.0) < 0.001)
    , testCase "wide parallel: A -> {B, C, D, E} -> F, B slowest" $ do
        -- F needs B,C,D,E; each needs A
        -- B is slowest at 10
        let graph =
              mkGraph
                [ (0, "A", 1)
                , (1, "B", 10)
                , (2, "C", 2)
                , (3, "D", 3)
                , (4, "E", 1)
                , (5, "F", 1)
                ]
                [ (1, 0)
                , (2, 0)
                , (3, 0)
                , (4, 0) -- B,C,D,E -> A
                , (5, 1)
                , (5, 2)
                , (5, 3)
                , (5, 4) -- F -> B,C,D,E
                ]
            result = computeCriticalPath graph
        assertEqual "path goes through B" [0, 1, 5] (graphCriticalPath result)
        assertBool "total duration is 12" (abs (criticalPathDuration result - 12.0) < 0.001)
    , testCase "single node" $ do
        let graph = mkGraph [(0, "only", 5.0)] []
            result = computeCriticalPath graph
        assertEqual "path" [0] (graphCriticalPath result)
        assertBool "total duration is 5" (abs (criticalPathDuration result - 5.0) < 0.001)
    , testCase "disconnected components: longer chain wins" $ do
        -- Chain 1: X(1) -> Y(2) = 3
        -- Chain 2: P(5) -> Q(6) = 11
        let graph =
              mkGraph
                [(0, "X", 1), (1, "Y", 2), (2, "P", 5), (3, "Q", 6)]
                [(1, 0), (3, 2)] -- Y->X, Q->P
            result = computeCriticalPath graph
        assertEqual "path is P -> Q" [2, 3] (graphCriticalPath result)
        assertBool "total duration is 11" (abs (criticalPathDuration result - 11.0) < 0.001)
    , testCase "empty graph" $ do
        let graph = mkGraph [] []
            result = computeCriticalPath graph
        assertEqual "empty path" [] (graphCriticalPath result)
    , testCase "nodes with no timing data contribute 0" $ do
        let graph =
              BuildGraph
                { graphNodes =
                    IntMap.fromList
                      [ (0, Node 0 "a" FileNode Nothing Nothing Nothing)
                      , (1, Node 1 "b" FileNode (Just 0) (Just 3) (Just 3))
                      ]
                , graphEdges = Vector.fromList [Edge 1 0]
                , graphBuildStart = UTCTime (fromGregorian 2024 1 1) 0
                , graphTotalSeconds = 0
                , graphCriticalPath = []
                }
            result = computeCriticalPath graph
        assertEqual "path" [0, 1] (graphCriticalPath result)
        assertBool "total duration is 3" (abs (criticalPathDuration result - 3.0) < 0.001)
    ]
