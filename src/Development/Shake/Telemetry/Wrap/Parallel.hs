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

parallel :: [Action a] -> Action [a]
parallel actions = do
  state <- getTelemetryState
  parentNode <- Shake.liftIO $ getThreadNode state
  Shake.parallel
    [ do Shake.liftIO $ setThreadNode state parentNode
         a
    | a <- actions
    ]

forP :: [a] -> (a -> Action b) -> Action [b]
forP xs f = do
  state <- getTelemetryState
  parentNode <- Shake.liftIO $ getThreadNode state
  Shake.forP xs $ \x -> do
    Shake.liftIO $ setThreadNode state parentNode
    f x

par :: Action a -> Action b -> Action (a, b)
par a b = do
  state <- getTelemetryState
  parentNode <- Shake.liftIO $ getThreadNode state
  Shake.par
    (Shake.liftIO (setThreadNode state parentNode) >> a)
    (Shake.liftIO (setThreadNode state parentNode) >> b)
