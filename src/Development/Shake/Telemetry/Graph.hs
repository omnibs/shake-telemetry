module Development.Shake.Telemetry.Graph
  ( NodeType (..)
  , Node (..)
  , Edge (..)
  , BuildGraph (..)
  ) where

import Data.IntMap.Strict (IntMap)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import GHC.Generics (Generic)

data NodeType
  = FileNode
  | PhonyNode
  | OracleNode
  | BatchNode
  | DirectoryNode
  | EnvNode
  | ActionNode
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)

data Node = Node
  { nodeId :: !Int
  , nodeLabel :: !Text
  , nodeType :: !NodeType
  , nodeStartTime :: !(Maybe Double)
  , nodeEndTime :: !(Maybe Double)
  , nodeDuration :: !(Maybe Double)
  }
  deriving (Eq, Show, Generic)

data Edge = Edge
  { edgeFrom :: !Int
  , edgeTo :: !Int
  }
  deriving (Eq, Show, Ord, Generic)

data BuildGraph = BuildGraph
  { graphNodes :: !(IntMap Node)
  , graphEdges :: !(Vector Edge)
  , graphBuildStart :: !UTCTime
  , graphTotalSeconds :: !Double
  , graphCriticalPath :: ![Int]
  }
  deriving (Eq, Show, Generic)
