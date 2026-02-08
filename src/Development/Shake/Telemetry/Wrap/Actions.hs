{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Development.Shake.Telemetry.Wrap.Actions
  ( need
  , needed
  , want
  , orderOnly
  , orderOnlyAction
  , askOracle
  , askOracles
  , askOracleWith
  , doesFileExist
  , doesDirectoryExist
  , getDirectoryContents
  , getDirectoryFiles
  , getDirectoryDirs
  , getEnv
  , getEnvWithDefault
  ) where

import Data.Text qualified as T
import GHC.Stack (HasCallStack)
import Development.Shake (Action, FilePattern, RuleResult, Rules, ShakeValue)
import Development.Shake qualified as Shake
import Development.Shake.Telemetry.Graph (NodeType (..))
import Development.Shake.Telemetry.State (TelemetryState, getThreadNode, recordEdge)

-- | Retrieve TelemetryState from shakeExtra inside an Action.
getTelemetryState :: Action TelemetryState
getTelemetryState = do
  mstate <- Shake.getShakeExtra
  case mstate of
    Just s -> pure s
    Nothing -> error "shake-telemetry: TelemetryState not found in shakeExtra"

-- | Record edges from the current thread's node to each target.
recordEdges :: TelemetryState -> NodeType -> [String] -> Action ()
recordEdges state ntype targets = do
  currentNode <- Shake.liftIO $ getThreadNode state
  mapM_ (\t -> Shake.liftIO $ recordEdge state currentNode (T.pack t) ntype) targets

-- Core file dependencies

need :: HasCallStack => [FilePath] -> Action ()
need files = do
  state <- getTelemetryState
  recordEdges state FileNode files
  Shake.need files

needed :: HasCallStack => [FilePath] -> Action ()
needed files = do
  state <- getTelemetryState
  recordEdges state FileNode files
  Shake.needed files

want :: HasCallStack => [FilePath] -> Rules ()
want = Shake.want

orderOnly :: [FilePath] -> Action ()
orderOnly files = do
  state <- getTelemetryState
  recordEdges state FileNode files
  Shake.orderOnly files

orderOnlyAction :: Action a -> Action a
orderOnlyAction = Shake.orderOnlyAction

-- Oracle dependencies

askOracle :: (RuleResult q ~ a, ShakeValue q, ShakeValue a) => q -> Action a
askOracle q = do
  state <- getTelemetryState
  currentNode <- Shake.liftIO $ getThreadNode state
  Shake.liftIO $ recordEdge state currentNode (T.pack (show q)) OracleNode
  Shake.askOracle q

askOracles :: (RuleResult q ~ a, ShakeValue q, ShakeValue a) => [q] -> Action [a]
askOracles qs = do
  state <- getTelemetryState
  currentNode <- Shake.liftIO $ getThreadNode state
  mapM_ (\q -> Shake.liftIO $ recordEdge state currentNode (T.pack (show q)) OracleNode) qs
  Shake.askOracles qs

askOracleWith :: (RuleResult q ~ a, ShakeValue q, ShakeValue a) => q -> a -> Action a
askOracleWith q a = do
  state <- getTelemetryState
  currentNode <- Shake.liftIO $ getThreadNode state
  Shake.liftIO $ recordEdge state currentNode (T.pack (show q)) OracleNode
  Shake.askOracleWith q a

-- Implicit file dependencies

doesFileExist :: FilePath -> Action Bool
doesFileExist path = do
  state <- getTelemetryState
  currentNode <- Shake.liftIO $ getThreadNode state
  Shake.liftIO $ recordEdge state currentNode (T.pack path) DirectoryNode
  Shake.doesFileExist path

doesDirectoryExist :: FilePath -> Action Bool
doesDirectoryExist path = do
  state <- getTelemetryState
  currentNode <- Shake.liftIO $ getThreadNode state
  Shake.liftIO $ recordEdge state currentNode (T.pack path) DirectoryNode
  Shake.doesDirectoryExist path

getDirectoryContents :: FilePath -> Action [FilePath]
getDirectoryContents path = do
  state <- getTelemetryState
  currentNode <- Shake.liftIO $ getThreadNode state
  Shake.liftIO $ recordEdge state currentNode (T.pack path) DirectoryNode
  Shake.getDirectoryContents path

getDirectoryFiles :: FilePath -> [FilePattern] -> Action [FilePath]
getDirectoryFiles dir pats = do
  state <- getTelemetryState
  currentNode <- Shake.liftIO $ getThreadNode state
  Shake.liftIO $ recordEdge state currentNode (T.pack dir) DirectoryNode
  Shake.getDirectoryFiles dir pats

getDirectoryDirs :: FilePath -> Action [FilePath]
getDirectoryDirs path = do
  state <- getTelemetryState
  currentNode <- Shake.liftIO $ getThreadNode state
  Shake.liftIO $ recordEdge state currentNode (T.pack path) DirectoryNode
  Shake.getDirectoryDirs path

-- Environment dependencies

getEnv :: String -> Action (Maybe String)
getEnv var = do
  state <- getTelemetryState
  currentNode <- Shake.liftIO $ getThreadNode state
  Shake.liftIO $ recordEdge state currentNode (T.pack var) EnvNode
  Shake.getEnv var

getEnvWithDefault :: String -> String -> Action String
getEnvWithDefault var def = do
  state <- getTelemetryState
  currentNode <- Shake.liftIO $ getThreadNode state
  Shake.liftIO $ recordEdge state currentNode (T.pack var) EnvNode
  Shake.getEnvWithDefault var def
