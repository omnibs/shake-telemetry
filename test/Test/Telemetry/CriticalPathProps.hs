{-# LANGUAGE OverloadedStrings #-}

module Test.Telemetry.CriticalPathProps (criticalPathProps) where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Time (UTCTime (..))
import Data.Time.Calendar (fromGregorian)
import Data.Vector qualified as Vector
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.Hedgehog

import Development.Shake.Telemetry.CriticalPath
import Development.Shake.Telemetry.Graph

-- | Generate a random DAG. Nodes are numbered 0..n-1. Edges only go from
-- higher-numbered nodes to lower-numbered nodes, guaranteeing a DAG.
genDAG :: Gen BuildGraph
genDAG = do
  n <- Gen.int (Range.linear 1 20)
  let nodeIds = [0 .. n - 1]
  durations <- traverse (\_ -> Gen.double (Range.linearFrac 0.0 10.0)) nodeIds
  let nodes =
        IntMap.fromList
          [ ( i
            , Node
                { nodeId = i
                , nodeLabel = T.pack ("n" ++ show i)
                , nodeType = FileNode
                , nodeStartTime = Just 0
                , nodeEndTime = Just d
                , nodeDuration = Just d
                }
            )
          | (i, d) <- zip nodeIds durations
          ]
  -- Generate edges: from higher to lower (ensures DAG)
  edges <- fmap concat $
    traverse
      ( \from ->
          if from == 0
            then pure []
            else do
              targets <- Gen.subsequence [0 .. from - 1]
              pure [Edge from to | to <- targets]
      )
      nodeIds
  pure
    BuildGraph
      { graphNodes = nodes
      , graphEdges = Vector.fromList edges
      , graphBuildStart = UTCTime (fromGregorian 2024 1 1) 0
      , graphTotalSeconds = 0
      , graphCriticalPath = []
      }

-- | Compute the duration of a path.
pathDuration :: BuildGraph -> [Int] -> Double
pathDuration graph path =
  sum [fromMaybe 0 (IntMap.lookup nid (graphNodes graph) >>= nodeDuration) | nid <- path]

-- | Check if consecutive nodes in the path are connected by edges.
isValidPath :: BuildGraph -> [Int] -> Bool
isValidPath graph path = all hasEdge (zip path (drop 1 path))
  where
    edgeSet = IntSet.fromList [edgeFrom e * 100000 + edgeTo e | e <- Vector.toList (graphEdges graph)]
    -- Edge from dependent to dependency. Path goes source -> ... -> sink.
    -- Consecutive (u, v) in path means v depends on u, so Edge(from=v, to=u).
    hasEdge (u, v) = IntSet.member (v * 100000 + u) edgeSet

-- | Brute-force all paths in the DAG and find the longest.
bruteForceLongestPath :: BuildGraph -> Double
bruteForceLongestPath graph
  | IntMap.null (graphNodes graph) = 0
  | otherwise = maximum (map (pathDuration graph) allPaths)
  where
    edges = Vector.toList (graphEdges graph)
    -- Successors in execution order: for Edge(from=w, to=v), w is successor of v
    succs :: IntMap.IntMap [Int]
    succs =
      foldl'
        (\acc (Edge from to) -> IntMap.adjust (from :) to acc)
        (IntMap.map (const []) (graphNodes graph))
        edges
    -- All paths starting from each node
    allPaths = concatMap pathsFrom (IntMap.keys (graphNodes graph))
    pathsFrom nid = case fromMaybe [] (IntMap.lookup nid succs) of
      [] -> [[nid]]
      ss -> [nid : p | s <- ss, p <- pathsFrom s]

criticalPathProps :: TestTree
criticalPathProps =
  testGroup
    "CriticalPath properties"
    [ testProperty "critical path is a valid path in the graph" $ property $ do
        graph <- forAll genDAG
        let result = computeCriticalPath graph
            cp = graphCriticalPath result
        case cp of
          [] -> success
          [_] -> success
          _ -> assert (isValidPath graph cp)
    , testProperty "all edge endpoints reference existing nodes" $ property $ do
        graph <- forAll genDAG
        let result = computeCriticalPath graph
            nodeIds = IntMap.keysSet (graphNodes result)
        annotateShow (graphCriticalPath result)
        assert $ all (`IntSet.member` nodeIds) (graphCriticalPath result)
    , testProperty "critical path duration <= sum of all durations" $ property $ do
        graph <- forAll genDAG
        let result = computeCriticalPath graph
            cpDur = pathDuration result (graphCriticalPath result)
            totalDur = sum [fromMaybe 0 (nodeDuration n) | n <- IntMap.elems (graphNodes result)]
        diff cpDur (<=) (totalDur + 0.001) -- small epsilon for FP
    , testProperty "critical path is optimal (brute force, small graphs)" $ property $ do
        graph <- forAll genDAG
        let result = computeCriticalPath graph
            cpDur = pathDuration result (graphCriticalPath result)
            bfDur = bruteForceLongestPath graph
        diff (abs (cpDur - bfDur)) (<) 0.001
    ]
