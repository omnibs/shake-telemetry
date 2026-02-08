{-# LANGUAGE OverloadedStrings #-}

module Test.Telemetry.WrapTest (wrapTests) where

import Data.IntMap.Strict qualified as IntMap
import Data.Vector qualified as Vector
import Development.Shake (ShakeOptions (..), shakeOptions)
import Development.Shake qualified as Shake
import Development.Shake.Telemetry qualified as T
import Development.Shake.Telemetry.Graph
import Development.Shake.Telemetry.State (TelemetryState, freezeGraph, newTelemetryState)
import System.FilePath ((-<.>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

-- | Helper: create ShakeOptions with telemetry state injected.
mkOpts :: FilePath -> TelemetryState -> ShakeOptions
mkOpts tmpDir state =
  shakeOptions
    { shakeFiles = tmpDir
    , shakeVerbosity = Shake.Silent
    , shakeExtra = Shake.addShakeExtra state (shakeExtra shakeOptions)
    }

wrapTests :: TestTree
wrapTests =
  testGroup
    "Wrap integration"
    [ testCase "file rule (%>) records FileNode" testFileRule
    , testCase "phony rule records PhonyNode" testPhonyRule
    , testCase "need records edges from rule to dependency" testNeedEdges
    , testCase "parallel preserves thread context" testParallelContext
    ]

testFileRule :: Assertion
testFileRule =
  withSystemTempDirectory "shake-test" $ \tmpDir -> do
    state <- newTelemetryState
    let opts = mkOpts tmpDir state
    Shake.shake opts $ do
      (tmpDir ++ "//*.txt") T.%> \out -> Shake.writeFile' out "content"
      Shake.want [tmpDir ++ "/test.txt"]
    graph <- freezeGraph state
    let nodes = IntMap.elems (graphNodes graph)
        fileNodes = filter (\n -> nodeType n == FileNode) nodes
    assertBool "at least one FileNode" (not (null fileNodes))
    -- The file node should have a duration (non-Nothing)
    let durations = map nodeDuration fileNodes
    assertBool "FileNode has duration" (any (/= Nothing) durations)

testPhonyRule :: Assertion
testPhonyRule =
  withSystemTempDirectory "shake-test" $ \tmpDir -> do
    state <- newTelemetryState
    let opts = mkOpts tmpDir state
    Shake.shake opts $ do
      T.phony "clean" $ pure ()
      Shake.action $ Shake.need ["clean"]
    graph <- freezeGraph state
    let nodes = IntMap.elems (graphNodes graph)
        phonyNodes = filter (\n -> nodeType n == PhonyNode) nodes
    assertBool "at least one PhonyNode" (not (null phonyNodes))

testNeedEdges :: Assertion
testNeedEdges =
  withSystemTempDirectory "shake-test" $ \tmpDir -> do
    state <- newTelemetryState
    let opts = mkOpts tmpDir state
    Shake.shake opts $ do
      -- Rule for .src files (the dependency)
      (tmpDir ++ "//*.src") T.%> \out -> Shake.writeFile' out "source"
      -- Rule for .out files that needs the .src file
      (tmpDir ++ "//*.out") T.%> \out -> do
        let src = out -<.> "src"
        T.need [src]
        Shake.writeFile' out "output"
      Shake.want [tmpDir ++ "/test.out"]
    graph <- freezeGraph state
    let edgeCount = Vector.length (graphEdges graph)
    assertBool ("edges exist, got " ++ show edgeCount) (edgeCount > 0)

testParallelContext :: Assertion
testParallelContext =
  withSystemTempDirectory "shake-test" $ \tmpDir -> do
    state <- newTelemetryState
    let opts = mkOpts tmpDir state
    Shake.shake opts $ do
      -- Rules for individual files
      (tmpDir ++ "//*.dep") T.%> \out -> Shake.writeFile' out "dep"
      -- Rule that uses parallel to need two dependencies
      (tmpDir ++ "//*.all") T.%> \out -> do
        _ <- T.parallel
          [ T.need [tmpDir ++ "/a.dep"]
          , T.need [tmpDir ++ "/b.dep"]
          ]
        Shake.writeFile' out "all"
      Shake.want [tmpDir ++ "/test.all"]
    graph <- freezeGraph state
    let edgeCount = Vector.length (graphEdges graph)
    assertBool ("parallel edges >= 2, got " ++ show edgeCount) (edgeCount >= 2)
