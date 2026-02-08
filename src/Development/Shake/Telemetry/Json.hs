{-# LANGUAGE OverloadedStrings #-}

module Development.Shake.Telemetry.Json
  ( writeJsonGraph
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, withText, (.:), (.:?), (.=))
import Data.Aeson.Types (Parser)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as Vector
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import Development.Shake.Telemetry.Graph

-- NodeType

instance ToJSON NodeType where
  toJSON FileNode = String "file"
  toJSON PhonyNode = String "phony"
  toJSON OracleNode = String "oracle"
  toJSON BatchNode = String "batch"
  toJSON DirectoryNode = String "directory"
  toJSON EnvNode = String "env"
  toJSON ActionNode = String "action"

instance FromJSON NodeType where
  parseJSON = withText "NodeType" $ \t -> case t of
    "file" -> pure FileNode
    "phony" -> pure PhonyNode
    "oracle" -> pure OracleNode
    "batch" -> pure BatchNode
    "directory" -> pure DirectoryNode
    "env" -> pure EnvNode
    "action" -> pure ActionNode
    _ -> fail $ "unknown NodeType: " ++ T.unpack t

-- Edge

instance ToJSON Edge where
  toJSON (Edge from to) = object ["from" .= from, "to" .= to]

instance FromJSON Edge where
  parseJSON = withObject "Edge" $ \o ->
    Edge <$> o .: "from" <*> o .: "to"

-- Node (standalone, without onCriticalPath — used for FromJSON)

instance FromJSON Node where
  parseJSON = withObject "Node" $ \o ->
    Node
      <$> o .: "id"
      <*> o .: "label"
      <*> o .: "type"
      <*> o .:? "startTime"
      <*> o .:? "endTime"
      <*> o .:? "duration"

-- | Encode a node with the onCriticalPath flag.
nodeToJsonWithCP :: IntSet.IntSet -> Node -> Value
nodeToJsonWithCP cpSet node =
  object
    [ "id" .= nodeId node
    , "label" .= nodeLabel node
    , "type" .= nodeType node
    , "startTime" .= nodeStartTime node
    , "endTime" .= nodeEndTime node
    , "duration" .= nodeDuration node
    , "onCriticalPath" .= IntSet.member (nodeId node) cpSet
    ]

-- BuildGraph

instance ToJSON BuildGraph where
  toJSON graph =
    let cpSet = IntSet.fromList (graphCriticalPath graph)
        nodesList = map snd (IntMap.toAscList (graphNodes graph))
     in object
          [ "buildStart" .= graphBuildStart graph
          , "totalSeconds" .= graphTotalSeconds graph
          , "nodes" .= map (nodeToJsonWithCP cpSet) nodesList
          , "edges" .= Vector.toList (graphEdges graph)
          , "criticalPath" .= graphCriticalPath graph
          ]

instance FromJSON BuildGraph where
  parseJSON = withObject "BuildGraph" $ \o -> do
    nodesList <- o .: "nodes" :: Parser [Node]
    edgesList <- o .: "edges" :: Parser [Edge]
    BuildGraph
      <$> pure (IntMap.fromList [(nodeId n, n) | n <- nodesList])
      <*> pure (Vector.fromList edgesList)
      <*> o .: "buildStart"
      <*> o .: "totalSeconds"
      <*> o .: "criticalPath"

-- | Write the build graph as pretty-printed JSON. Creates parent directories.
writeJsonGraph :: FilePath -> BuildGraph -> IO ()
writeJsonGraph path graph = do
  createDirectoryIfMissing True (takeDirectory path)
  BSL.writeFile path (Aeson.encodePretty graph)
