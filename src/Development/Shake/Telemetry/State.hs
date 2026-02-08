module Development.Shake.Telemetry.State
  ( TelemetryState (..)
  , newTelemetryState
  , getOrCreateNode
  , registerNode
  , finishNode
  , recordEdge
  , setThreadNode
  , getThreadNode
  , freezeGraph
  ) where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Concurrent.Class.MonadSTM.Strict
  ( MonadSTM (..)
  , StrictTVar
  , atomically
  , modifyTVar
  , newTVarIO
  , readTVar
  , readTVarIO
  , stateTVar
  , writeTVar
  )
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Data.Vector qualified as Vector
import System.Clock (Clock (Monotonic), TimeSpec, getTime, toNanoSecs)

import Development.Shake.Telemetry.Graph

data TelemetryState = TelemetryState
  { tsNodes :: !(StrictTVar IO (IntMap Node))
  , tsEdges :: !(StrictTVar IO [Edge])
  , tsNextId :: !(StrictTVar IO Int)
  , tsThreadContext :: !(StrictTVar IO (Map ThreadId Int))
  , tsBuildStart :: !TimeSpec
  , tsLabelToId :: !(StrictTVar IO (HashMap Text Int))
  }

newTelemetryState :: IO TelemetryState
newTelemetryState = do
  nodes <- newTVarIO IntMap.empty
  edges <- newTVarIO []
  nextId <- newTVarIO 0
  threadCtx <- newTVarIO Map.empty
  start <- getTime Monotonic
  labelToId <- newTVarIO HashMap.empty
  pure
    TelemetryState
      { tsNodes = nodes
      , tsEdges = edges
      , tsNextId = nextId
      , tsThreadContext = threadCtx
      , tsBuildStart = start
      , tsLabelToId = labelToId
      }

-- | Get elapsed seconds since build start.
elapsedSeconds :: TelemetryState -> IO Double
elapsedSeconds state = do
  now <- getTime Monotonic
  let nanos = toNanoSecs (now - tsBuildStart state)
  pure (fromIntegral nanos / 1e9)

-- | Get or create a node by label. Returns the node ID.
-- If the node already exists, returns its existing ID.
-- If not, creates a placeholder node (no timing data) and returns the new ID.
getOrCreateNode :: TelemetryState -> Text -> NodeType -> IO Int
getOrCreateNode state label nodeType = atomically $ do
  labelMap <- readTVar (tsLabelToId state)
  case HashMap.lookup label labelMap of
    Just nid -> pure nid
    Nothing -> do
      nid <- stateTVar (tsNextId state) (\n -> (n, n + 1))
      let node =
            Node
              { nodeId = nid
              , nodeLabel = label
              , nodeType = nodeType
              , nodeStartTime = Nothing
              , nodeEndTime = Nothing
              , nodeDuration = Nothing
              }
      modifyTVar (tsNodes state) (IntMap.insert nid node)
      modifyTVar (tsLabelToId state) (HashMap.insert label nid)
      pure nid

-- | Register a node and record its start time.
-- Uses getOrCreateNode for deduplication, then stamps the start time.
registerNode :: TelemetryState -> Text -> NodeType -> IO Int
registerNode state label nodeType = do
  nid <- getOrCreateNode state label nodeType
  elapsed <- elapsedSeconds state
  atomically $
    modifyTVar (tsNodes state) $
      IntMap.adjust (\n -> n {nodeStartTime = Just elapsed}) nid
  pure nid

-- | Record the end time and compute duration for a node.
finishNode :: TelemetryState -> Int -> IO ()
finishNode state nid = do
  elapsed <- elapsedSeconds state
  atomically $
    modifyTVar (tsNodes state) $
      IntMap.adjust
        ( \n ->
            n
              { nodeEndTime = Just elapsed
              , nodeDuration = Just (elapsed - maybe 0 id (nodeStartTime n))
              }
        )
        nid

-- | Record an edge from a source node to a target identified by label.
-- Creates the target node if it doesn't exist yet.
recordEdge :: TelemetryState -> Int -> Text -> NodeType -> IO ()
recordEdge state fromId targetLabel targetType = do
  toId <- getOrCreateNode state targetLabel targetType
  atomically $
    modifyTVar (tsEdges state) (Edge {edgeFrom = fromId, edgeTo = toId} :)

-- | Associate the current thread with a node ID.
setThreadNode :: TelemetryState -> Int -> IO ()
setThreadNode state nid = do
  tid <- myThreadId
  atomically $ modifyTVar (tsThreadContext state) (Map.insert tid nid)

-- | Get the node ID associated with the current thread.
-- Throws an error if no context is set (indicates a wrapping bug).
getThreadNode :: TelemetryState -> IO Int
getThreadNode state = do
  tid <- myThreadId
  ctx <- readTVarIO (tsThreadContext state)
  case Map.lookup tid ctx of
    Just nid -> pure nid
    Nothing -> error $ "shake-telemetry: no thread context for " ++ show tid

-- | Freeze the mutable state into an immutable BuildGraph.
freezeGraph :: TelemetryState -> IO BuildGraph
freezeGraph state = do
  nodes <- readTVarIO (tsNodes state)
  edgeList <- readTVarIO (tsEdges state)
  now <- getCurrentTime
  elapsed <- elapsedSeconds state
  pure
    BuildGraph
      { graphNodes = nodes
      , graphEdges = Vector.fromList edgeList
      , graphBuildStart = now
      , graphTotalSeconds = elapsed
      , graphCriticalPath = []
      }
