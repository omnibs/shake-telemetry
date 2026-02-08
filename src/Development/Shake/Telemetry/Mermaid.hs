{-# LANGUAGE OverloadedStrings #-}

module Development.Shake.Telemetry.Mermaid
  ( renderMermaid
  , writeMermaidChart
  ) where

import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import Development.Shake.Telemetry.Graph

-- | Render a Mermaid graph LR diagram showing only the critical path.
renderMermaid :: BuildGraph -> Text
renderMermaid graph =
  T.unlines $ filter (not . T.null) $
    [ "graph LR"
    , "    classDef critical fill:#ff6b6b,stroke:#333,stroke-width:2px"
    ]
    ++ nodeEdgeLines
    ++ classLine
  where
    cp = graphCriticalPath graph
    nodeMap = graphNodes graph

    nodeLabel :: Int -> Text
    nodeLabel nid =
      case IntMap.lookup nid nodeMap of
        Just n ->
          let dur = fromMaybe 0 (nodeDuration n)
              durText = T.pack (show dur) <> "s"
          in "n" <> T.pack (show nid) <> "[\"" <> Development.Shake.Telemetry.Graph.nodeLabel n <> " (" <> durText <> ")\"]"
        Nothing ->
          "n" <> T.pack (show nid) <> "[\"unknown\"]"

    nodeEdgeLines :: [Text]
    nodeEdgeLines = case cp of
      [] -> []
      [single] -> ["    " <> nodeLabel single]
      _ -> [ "    " <> nodeLabel a <> " --> " <> nodeLabel b
           | (a, b) <- zip cp (drop 1 cp)
           ]

    classLine :: [Text]
    classLine = case cp of
      [] -> []
      _ -> ["    class " <> T.intercalate "," (map (\nid -> "n" <> T.pack (show nid)) cp) <> " critical"]

-- | Write the Mermaid chart to a file. Creates parent directories.
writeMermaidChart :: FilePath -> BuildGraph -> IO ()
writeMermaidChart path graph = do
  createDirectoryIfMissing True (takeDirectory path)
  TIO.writeFile path (renderMermaid graph)
