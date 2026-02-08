{-# LANGUAGE OverloadedStrings #-}

-- | This module verifies that Development.Shake.Telemetry exports
-- the same API surface as Development.Shake. It imports both modules
-- qualified and binds a representative sample of functions from each,
-- asserting they have compatible types. If any export is missing from
-- our module, this file will fail to compile.
module Test.Telemetry.ParityTest (parityTests) where

import Development.Shake qualified as S
import Development.Shake.Telemetry qualified as T
import Test.Tasty
import Test.Tasty.HUnit

-- Type witnesses: if these compile, the types match.
-- We don't need to run them, just verify they type-check.

-- Entry points
_shake :: S.ShakeOptions -> S.Rules () -> IO ()
_shake = T.shake

_shakeArgs :: S.ShakeOptions -> S.Rules () -> IO ()
_shakeArgs = T.shakeArgs

-- Rule definers
_fileRule :: S.FilePattern -> (FilePath -> S.Action ()) -> S.Rules ()
_fileRule = (T.%>)

_multiFileRule :: [S.FilePattern] -> (FilePath -> S.Action ()) -> S.Rules ()
_multiFileRule = (T.|%>)

_predFileRule :: (FilePath -> Bool) -> (FilePath -> S.Action ()) -> S.Rules ()
_predFileRule = (T.?>)

_batchFileRule :: [S.FilePattern] -> ([FilePath] -> S.Action ()) -> S.Rules ()
_batchFileRule = (T.&%>)

_phony :: String -> S.Action () -> S.Rules ()
_phony = T.phony

_phonyOp :: String -> S.Action () -> S.Rules ()
_phonyOp = (T.~>)

-- Dependency creators
_need :: [FilePath] -> S.Action ()
_need = T.need

_needed :: [FilePath] -> S.Action ()
_needed = T.needed

_want :: [FilePath] -> S.Rules ()
_want = T.want

_orderOnly :: [FilePath] -> S.Action ()
_orderOnly = T.orderOnly

_doesFileExist :: FilePath -> S.Action Bool
_doesFileExist = T.doesFileExist

_doesDirectoryExist :: FilePath -> S.Action Bool
_doesDirectoryExist = T.doesDirectoryExist

_getDirectoryContents :: FilePath -> S.Action [FilePath]
_getDirectoryContents = T.getDirectoryContents

_getDirectoryDirs :: FilePath -> S.Action [FilePath]
_getDirectoryDirs = T.getDirectoryDirs

_getEnv :: String -> S.Action (Maybe String)
_getEnv = T.getEnv

-- Parallelism
_parallel :: [S.Action a] -> S.Action [a]
_parallel = T.parallel

_forP :: [a] -> (a -> S.Action b) -> S.Action [b]
_forP = T.forP

-- Re-exported types (just verify they exist)
_shakeOptions :: S.ShakeOptions
_shakeOptions = T.shakeOptions

-- Re-exported functions
_action :: S.Action a -> S.Rules ()
_action = T.action

_copyFile :: FilePath -> FilePath -> S.Action ()
_copyFile = T.copyFile'

_readFile :: FilePath -> S.Action String
_readFile = T.readFile'

_writeFile :: FilePath -> String -> S.Action ()
_writeFile = T.writeFile'

_newCache :: (T.Hashable a, Eq a) => (a -> S.Action b) -> S.Rules (a -> S.Action b)
_newCache = T.newCache

_traced :: String -> IO a -> S.Action a
_traced = T.traced

_getVerbosity :: S.Action S.Verbosity
_getVerbosity = T.getVerbosity

_putInfo :: String -> S.Action ()
_putInfo = T.putInfo

_alwaysRerun :: S.Action ()
_alwaysRerun = T.alwaysRerun

_trackRead :: [FilePath] -> S.Action ()
_trackRead = T.trackRead

_progressSimple :: IO S.Progress -> IO ()
_progressSimple = T.progressSimple

-- Command execution
_cmd :: S.CmdResult r => [S.CmdOption] -> String -> [String] -> S.Action r
_cmd opts prog args = T.command opts prog args

parityTests :: TestTree
parityTests =
  testGroup
    "API parity"
    [ testCase "Development.Shake.Telemetry exports match Development.Shake" $
        -- If this module compiles, the API surface is compatible.
        -- This test just confirms the module loaded successfully.
        assertBool "API parity verified at compile time" True
    ]
