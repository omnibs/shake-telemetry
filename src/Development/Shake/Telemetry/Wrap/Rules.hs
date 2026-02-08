{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Development.Shake.Telemetry.Wrap.Rules
  ( (%>)
  , (|%>)
  , (?>)
  , (&%>)
  , (&?>)
  , phony
  , (~>)
  , phonys
  , addOracle
  , addOracleCache
  , addOracleHash
  , batch
  ) where

import Data.Text qualified as T
import GHC.Stack (HasCallStack)
import Development.Shake (Action, FilePattern, RuleResult, Rules, ShakeValue)
import Development.Shake qualified as Shake
import Development.Shake.Telemetry.Graph (NodeType (..))
import Development.Shake.Telemetry.State (TelemetryState, finishNode, registerNode, setThreadNode)

-- | Retrieve TelemetryState from shakeExtra inside an Action.
getTelemetryState :: Action TelemetryState
getTelemetryState = do
  mstate <- Shake.getShakeExtra
  case mstate of
    Just s -> pure s
    Nothing -> error "shake-telemetry: TelemetryState not found in shakeExtra"

-- | Wrap a (FilePath -> Action ()) body with telemetry.
wrapFileAction :: NodeType -> (FilePath -> Action ()) -> FilePath -> Action ()
wrapFileAction ntype act out = do
  state <- getTelemetryState
  nid <- Shake.liftIO $ registerNode state (T.pack out) ntype
  Shake.liftIO $ setThreadNode state nid
  act out
  Shake.liftIO $ finishNode state nid

-- | Wrap a ([FilePath] -> Action ()) body with telemetry (for batch/multi rules).
wrapBatchAction :: NodeType -> ([FilePath] -> Action ()) -> [FilePath] -> Action ()
wrapBatchAction ntype act outs = do
  state <- getTelemetryState
  let label = T.intercalate ", " (map T.pack outs)
  nid <- Shake.liftIO $ registerNode state label ntype
  Shake.liftIO $ setThreadNode state nid
  act outs
  Shake.liftIO $ finishNode state nid

-- | Wrap a plain Action () with telemetry (for phony).
wrapAction :: NodeType -> String -> Action () -> Action ()
wrapAction ntype name act = do
  state <- getTelemetryState
  nid <- Shake.liftIO $ registerNode state (T.pack name) ntype
  Shake.liftIO $ setThreadNode state nid
  act
  Shake.liftIO $ finishNode state nid

-- Match Shake's fixity declarations for all rule operators
infix 1 %>
infix 1 |%>
infix 1 ?>
infix 1 &%>
infix 1 &?>
infix 1 ~>

-- File rules

(%>) :: HasCallStack => FilePattern -> (FilePath -> Action ()) -> Rules ()
pat %> act = pat Shake.%> wrapFileAction FileNode act

(|%>) :: HasCallStack => [FilePattern] -> (FilePath -> Action ()) -> Rules ()
pats |%> act = pats Shake.|%> wrapFileAction FileNode act

(?>) :: HasCallStack => (FilePath -> Bool) -> (FilePath -> Action ()) -> Rules ()
test ?> act = test Shake.?> wrapFileAction FileNode act

-- Batch file rules

(&%>) :: HasCallStack => [FilePattern] -> ([FilePath] -> Action ()) -> Rules ()
pats &%> act = pats Shake.&%> wrapBatchAction BatchNode act

(&?>) :: HasCallStack => (FilePath -> Maybe [FilePath]) -> ([FilePath] -> Action ()) -> Rules ()
test &?> act = test Shake.&?> wrapBatchAction BatchNode act

-- Phony rules

phony :: HasCallStack => String -> Action () -> Rules ()
phony name act = Shake.phony name (wrapAction PhonyNode name act)

(~>) :: HasCallStack => String -> Action () -> Rules ()
name ~> act = name Shake.~> wrapAction PhonyNode name act

phonys :: HasCallStack => (String -> Maybe (Action ())) -> Rules ()
phonys test = Shake.phonys $ \name ->
  case test name of
    Nothing -> Nothing
    Just act -> Just (wrapAction PhonyNode name act)

-- Oracle rules

addOracle :: (RuleResult q ~ a, ShakeValue q, ShakeValue a, HasCallStack) => (q -> Action a) -> Rules (q -> Action a)
addOracle handler = Shake.addOracle $ \q -> do
  state <- getTelemetryState
  let label = T.pack (show q)
  nid <- Shake.liftIO $ registerNode state label OracleNode
  Shake.liftIO $ setThreadNode state nid
  result <- handler q
  Shake.liftIO $ finishNode state nid
  pure result

addOracleCache :: (RuleResult q ~ a, ShakeValue q, ShakeValue a, HasCallStack) => (q -> Action a) -> Rules (q -> Action a)
addOracleCache handler = Shake.addOracleCache $ \q -> do
  state <- getTelemetryState
  let label = T.pack (show q)
  nid <- Shake.liftIO $ registerNode state label OracleNode
  Shake.liftIO $ setThreadNode state nid
  result <- handler q
  Shake.liftIO $ finishNode state nid
  pure result

addOracleHash :: (RuleResult q ~ a, ShakeValue q, ShakeValue a, HasCallStack) => (q -> Action a) -> Rules (q -> Action a)
addOracleHash handler = Shake.addOracleHash $ \q -> do
  state <- getTelemetryState
  let label = T.pack (show q)
  nid <- Shake.liftIO $ registerNode state label OracleNode
  Shake.liftIO $ setThreadNode state nid
  result <- handler q
  Shake.liftIO $ finishNode state nid
  pure result

-- Batch

batch :: Int -> ((a -> Action ()) -> Rules ()) -> (a -> Action b) -> ([b] -> Action ()) -> Rules ()
batch maxBatch rules act collect = Shake.batch maxBatch rules wrappedAct wrappedCollect
  where
    wrappedAct a = do
      state <- getTelemetryState
      nid <- Shake.liftIO $ registerNode state "batch-item" BatchNode
      Shake.liftIO $ setThreadNode state nid
      result <- act a
      Shake.liftIO $ finishNode state nid
      pure result
    wrappedCollect bs = do
      state <- getTelemetryState
      nid <- Shake.liftIO $ registerNode state "batch-collect" BatchNode
      Shake.liftIO $ setThreadNode state nid
      collect bs
      Shake.liftIO $ finishNode state nid
