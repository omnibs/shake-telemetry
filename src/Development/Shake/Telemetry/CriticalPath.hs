module Development.Shake.Telemetry.CriticalPath
  ( computeCriticalPath
  ) where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Vector qualified as Vector

import Development.Shake.Telemetry.Graph

-- | Compute the critical path and annotate the graph with it.
-- The critical path is the longest-duration path through the DAG.
computeCriticalPath :: BuildGraph -> BuildGraph
computeCriticalPath graph =
  graph {graphCriticalPath = path}
  where
    nodes = graphNodes graph
    edges = Vector.toList (graphEdges graph)

    -- Build adjacency list: for each node, which nodes depend on it (successors)
    -- and which nodes it depends on (predecessors).
    predecessors :: IntMap [Int]
    predecessors = foldl' addPred (IntMap.map (const []) nodes) edges
      where
        addPred acc (Edge from to) = IntMap.adjust (from :) to acc

    -- In-degree for Kahn's algorithm
    inDegree :: IntMap Int
    inDegree = foldl' addDeg (IntMap.map (const 0) nodes) edges
      where
        addDeg acc (Edge _from to) = IntMap.adjust (+ 1) to acc

    -- Successors: from -> [to]
    successors :: IntMap [Int]
    successors = foldl' addSucc (IntMap.map (const []) nodes) edges
      where
        addSucc acc (Edge from to) = IntMap.adjust (to :) from acc

    -- Kahn's topological sort
    topoSort :: [Int]
    topoSort = go initialQueue inDegree []
      where
        initialQueue = IntMap.keys (IntMap.filter (== 0) inDegree)
        go [] _ acc = reverse acc
        go (n : rest) deg acc =
          let succs = fromMaybe [] (IntMap.lookup n successors)
              (deg', newReady) = foldl' relax (deg, []) succs
              relax (d, ready) s =
                let d' = IntMap.adjust (subtract 1) s d
                    newDeg = fromMaybe 0 (IntMap.lookup s d')
                 in if newDeg == 0
                      then (d', s : ready)
                      else (d', ready)
           in go (newReady ++ rest) deg' (n : acc)

    -- Duration of a node (0 if no timing data)
    duration :: Int -> Double
    duration nid = fromMaybe 0 (IntMap.lookup nid nodes >>= nodeDuration)

    -- DP: earliest finish time for each node
    -- EFT(v) = duration(v) + max { EFT(u) | u is a predecessor of v } (0 if no preds)
    -- Also track which predecessor gave the max (for path reconstruction)
    eftMap :: IntMap (Double, Maybe Int)
    eftMap = foldl' step IntMap.empty topoSort
      where
        step acc nid =
          let preds = fromMaybe [] (IntMap.lookup nid predecessors)
              (maxPredEft, bestPred) = case preds of
                [] -> (0.0, Nothing)
                (p0 : ps) ->
                  let predEfts = [(fst (acc IntMap.! p), p) | p <- p0 : ps]
                      initial = case predEfts of
                        (x : _) -> x
                        _ -> error "impossible: non-empty preds"
                      (bestEft, bestP) = foldl' maxBy initial (drop 1 predEfts)
                      maxBy a@(ea, _) b@(eb, _) = if eb > ea then b else a
                   in (bestEft, Just bestP)
              eft = duration nid + maxPredEft
           in IntMap.insert nid (eft, bestPred) acc

    -- Find the sink (node with maximum EFT)
    path :: [Int]
    path
      | IntMap.null eftMap = []
      | otherwise =
          let (sinkId, _) = foldl' maxBy (IntMap.findMin eftMap) (IntMap.toList eftMap)
              maxBy a@(_, (ea, _)) b@(_, (eb, _)) = if eb > ea then b else a
              -- Trace back from sink
              traceback nid = nid : case snd (eftMap IntMap.! nid) of
                Nothing -> []
                Just predId -> traceback predId
           in reverse (traceback sinkId)
