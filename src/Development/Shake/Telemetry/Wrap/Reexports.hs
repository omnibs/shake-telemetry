module Development.Shake.Telemetry.Wrap.Reexports
  ( -- * Core types
    Rules
  , Action
  , ShakeOptions (..)
  , shakeOptions
  , Rebuild (..)
  , Lint (..)
  , Change (..)
  , Verbosity (..)
  , Progress (..)
  , Resource
  , ShakeException (..)
  , ShakeValue
  , RuleResult

  -- * Rule modifiers
  , action
  , alternatives
  , priority
  , versioned
  , withoutActions

  -- * File operations
  , copyFile'
  , copyFileChanged
  , readFile'
  , readFileLines
  , writeFile'
  , writeFileLines
  , writeFileChanged
  , removeFiles
  , removeFilesAfter
  , withTempFile
  , withTempDir
  , withTempFileWithin
  , withTempDirWithin
  , getDirectoryFilesIO

  -- * Tracking
  , trackRead
  , trackWrite
  , trackAllow
  , needHasChanged
  , resultHasChanged

  -- * Command execution
  , cmd
  , cmd_
  , command
  , command_
  , CmdOption (..)
  , CmdResult
  , CmdString
  , Stdout (..)
  , StdoutTrim (..)
  , Stderr (..)
  , Stdouterr (..)
  , Exit (..)
  , Process (..)
  , CmdTime (..)
  , CmdLine (..)
  , FSATrace (..)

  -- * Resources
  , newResource
  , newResourceIO
  , withResource
  , withResources
  , newThrottle
  , newThrottleIO
  , unsafeExtraThread

  -- * Caching
  , newCache
  , newCacheIO
  , historyDisable
  , produces
  , reschedule
  , deprioritize

  -- * Verbosity
  , getVerbosity
  , putVerbose
  , putInfo
  , putWarn
  , putError
  , withVerbosity
  , quietly
  , putLoud
  , putNormal
  , putQuiet

  -- * Progress
  , progressSimple
  , progressDisplay
  , progressTitlebar
  , progressProgram
  , getProgress

  -- * Targets
  , getTargets
  , addTarget
  , withTargetDocs
  , withoutTargets
  , addHelpSuffix

  -- * Options
  , getShakeOptions
  , getShakeOptionsRules
  , getHashedShakeVersion
  , getShakeExtra
  , getShakeExtraRules
  , addShakeExtra
  , shakeOptDescrs

  -- * Exception handling
  , actionOnException
  , actionFinally
  , actionBracket
  , actionCatch
  , actionRetry
  , runAfter

  -- * Misc
  , traced
  , unit
  , alwaysRerun

  -- * Environment (getEnvError only -- getEnv/getEnvWithDefault wrapped in Actions)
  , getEnvError

  -- * FilePattern (re-exported from Development.Shake.FilePath)
  , FilePattern
  , (?==)
  , (<//>)
  , filePattern

  -- * Re-exported classes (from Development.Shake.Classes)
  , Typeable
  , Hashable
  , Binary
  , NFData

  -- * Re-exported from standard libraries
  , OptDescr (..)
  , ArgDescr (..)
  , ExitCode (..)
  ) where

import Development.Shake
import Development.Shake.Classes (Binary, Hashable, NFData, Typeable)
import System.Console.GetOpt (ArgDescr (..), OptDescr (..))
import System.Exit (ExitCode (..))
