{-# LANGUAGE OverloadedStrings #-}

module Test.Telemetry.IntegrationTest (integrationTests) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.IntMap.Strict qualified as IntMap
import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as Vector
import Development.Shake (ShakeOptions (..), shakeOptions)
import Development.Shake qualified as Shake
import Development.Shake.Telemetry.CriticalPath (computeCriticalPath)
import Development.Shake.Telemetry.Graph
import Development.Shake.Telemetry.State (TelemetryState, freezeGraph, newTelemetryState)
import Development.Shake.Telemetry.Wrap.Actions qualified as WA
import Development.Shake.Telemetry.Wrap.Entry qualified as TE
import Development.Shake.Telemetry.Wrap.Parallel qualified as WP
import Development.Shake.Telemetry.Wrap.Rules qualified as WR
import System.Directory (doesFileExist)
import System.FilePath ((</>), (-<.>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

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
            , shakeVerbosity = Shake.Silent
            , shakeExtra = Shake.addShakeExtra state (shakeExtra shakeOptions)
            }
    buildFn tmpDir opts
    graph <- freezeGraph state
    let analyzed = computeCriticalPath graph
    checkFn tmpDir analyzed

-- | Helper to run a plain Shake build.
runBuild :: ShakeOptions -> Shake.Rules () -> IO ()
runBuild = Shake.shake

-- | Find all nodes of a given type.
nodesOfType :: NodeType -> BuildGraph -> [Node]
nodesOfType ntype graph =
  filter (\n -> nodeType n == ntype) (IntMap.elems (graphNodes graph))

-- | Find a node by label substring.
findNodeByLabel :: Text -> BuildGraph -> Maybe Node
findNodeByLabel substr graph =
  let nodes = IntMap.elems (graphNodes graph)
  in case filter (\n -> substr `T.isInfixOf` nodeLabel n) nodes of
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

integrationTests :: TestTree
integrationTests =
  testGroup
    "Integration"
    []
