module Development.Shake.Telemetry.Wrap.Parallel
  ( parallel
  , forP
  , par
  ) where

import Development.Shake (Action)
import Development.Shake qualified as Shake
import Development.Shake.Telemetry.State (TelemetryState, getThreadNode, setThreadNode)

-- | Retrieve TelemetryState from shakeExtra inside an Action.
getTelemetryState :: Action TelemetryState
getTelemetryState = do
  mstate <- Shake.getShakeExtra
  case mstate of
    Just s -> pure s
    Nothing -> error "shake-telemetry: TelemetryState not found in shakeExtra"

-- | Propagate the parent thread's node context to child threads, if any.
propagateContext :: TelemetryState -> Maybe Int -> Action a -> Action a
propagateContext state mNode act = do
  case mNode of
    Just nid -> Shake.liftIO $ setThreadNode state nid
    Nothing -> pure ()
  act

parallel :: [Action a] -> Action [a]
parallel actions = do
  state <- getTelemetryState
  mParent <- Shake.liftIO $ getThreadNode state
  Shake.parallel
    [ propagateContext state mParent a
    | a <- actions
    ]

forP :: [a] -> (a -> Action b) -> Action [b]
forP xs f = do
  state <- getTelemetryState
  mParent <- Shake.liftIO $ getThreadNode state
  Shake.forP xs $ \x ->
    propagateContext state mParent (f x)

par :: Action a -> Action b -> Action (a, b)
par a b = do
  state <- getTelemetryState
  mParent <- Shake.liftIO $ getThreadNode state
  Shake.par
    (propagateContext state mParent a)
    (propagateContext state mParent b)
