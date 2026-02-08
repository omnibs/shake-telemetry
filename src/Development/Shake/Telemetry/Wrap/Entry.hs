module Development.Shake.Telemetry.Wrap.Entry
  ( shake
  , shakeArgs
  , shakeArgsWith
  , shakeArgsOptionsWith
  ) where

import Development.Shake (Rules, ShakeOptions (..))
import Development.Shake qualified as Shake
import System.Console.GetOpt (OptDescr)

import Development.Shake.Telemetry.CriticalPath (computeCriticalPath)
import Development.Shake.Telemetry.Json (writeJsonGraph)
import Development.Shake.Telemetry.Mermaid (writeMermaidChart)
import Development.Shake.Telemetry.State (TelemetryState, freezeGraph, newTelemetryState)

-- | Inject TelemetryState into ShakeOptions.
injectTelemetry :: TelemetryState -> ShakeOptions -> ShakeOptions
injectTelemetry state opts =
  opts {shakeExtra = Shake.addShakeExtra state (shakeExtra opts)}

-- | Output paths derived from shakeFiles.
telemetryJsonPath :: ShakeOptions -> FilePath
telemetryJsonPath opts = shakeFiles opts ++ "/telemetry/build-graph.json"

telemetryMermaidPath :: ShakeOptions -> FilePath
telemetryMermaidPath opts = shakeFiles opts ++ "/telemetry/critical-path.mmd"

-- | Post-build: freeze graph, compute critical path, write outputs.
finalizeTelemetry :: ShakeOptions -> TelemetryState -> IO ()
finalizeTelemetry opts state = do
  graph <- freezeGraph state
  let analyzed = computeCriticalPath graph
  writeJsonGraph (telemetryJsonPath opts) analyzed
  writeMermaidChart (telemetryMermaidPath opts) analyzed

shake :: ShakeOptions -> Rules () -> IO ()
shake opts rules = do
  state <- newTelemetryState
  let opts' = injectTelemetry state opts
  Shake.shake opts' rules
  finalizeTelemetry opts' state

shakeArgs :: ShakeOptions -> Rules () -> IO ()
shakeArgs opts rules = do
  state <- newTelemetryState
  let opts' = injectTelemetry state opts
  Shake.shakeArgs opts' rules
  finalizeTelemetry opts' state

shakeArgsWith :: ShakeOptions -> [OptDescr (Either String a)] -> ([a] -> [String] -> IO (Maybe (Rules ()))) -> IO ()
shakeArgsWith opts flags act = do
  state <- newTelemetryState
  let opts' = injectTelemetry state opts
  Shake.shakeArgsWith opts' flags act
  finalizeTelemetry opts' state

shakeArgsOptionsWith :: ShakeOptions -> [OptDescr (Either String a)] -> (ShakeOptions -> [a] -> [String] -> IO (Maybe (ShakeOptions, Rules ()))) -> IO ()
shakeArgsOptionsWith opts flags act = do
  state <- newTelemetryState
  let opts' = injectTelemetry state opts
  Shake.shakeArgsOptionsWith opts' flags $ \o a s -> do
    result <- act o a s
    case result of
      Nothing -> pure Nothing
      Just (userOpts, rules) -> pure $ Just (injectTelemetry state userOpts, rules)
  finalizeTelemetry opts' state
