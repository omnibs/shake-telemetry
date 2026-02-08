module Development.Shake.Telemetry
  ( -- Re-export everything from Reexports (unchanged Shake API)
    module Development.Shake.Telemetry.Wrap.Reexports

    -- Wrapped entry points
  , shake
  , shakeArgs
  , shakeArgsWith
  , shakeArgsOptionsWith

    -- Wrapped rule definers
  , (%>)
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

    -- Wrapped dependency creators
  , need
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

    -- Wrapped parallelism
  , parallel
  , forP
  , par
  ) where

import Development.Shake.Telemetry.Wrap.Actions
import Development.Shake.Telemetry.Wrap.Entry
import Development.Shake.Telemetry.Wrap.Parallel
import Development.Shake.Telemetry.Wrap.Reexports
import Development.Shake.Telemetry.Wrap.Rules
