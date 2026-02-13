{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Telemetry.ThreadContextLossTest (threadContextLossTests) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.IntMap.Strict qualified as IntMap
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Development.Shake qualified as Shake (shake)
import Development.Shake.Classes (Binary, Hashable, NFData)
import Development.Shake.Telemetry qualified as T
import Development.Shake.Telemetry (ShakeOptions (..), shakeOptions)
import Development.Shake.Telemetry.CriticalPath (computeCriticalPath)
import Development.Shake.Telemetry.Graph
import Development.Shake.Telemetry.State (freezeGraph, newTelemetryState)
import GHC.Generics (Generic)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import Data.Typeable (Typeable)

newtype ContextOracleQ = ContextOracleQ String deriving (Eq, Show, Typeable, Generic)

instance Hashable ContextOracleQ
instance Binary ContextOracleQ
instance NFData ContextOracleQ
type instance T.RuleResult ContextOracleQ = String

threadContextLossTests :: TestTree
threadContextLossTests =
  testGroup
    "Thread context loss (expected to fail today)"
    [ testCase "apply/need: sequential needs after suspension keep all edges" testNeedAfterSuspension
    , testCase "apply/askOracle: sequential oracle queries after suspension keep all edges" testAskOracleAfterSuspension
    , testCase "parallel: parent keeps context after children complete" testParallelParentResume
    , testCase "forP: parent keeps context after children complete" testForPParentResume
    , testCase "par: parent keeps context after children complete" testParParentResume
    , testCase "batch: dependency recorded during collect is attributed to waiting item" testBatchItemResume
    ]

withTelemetryBuild
  :: String
  -> (FilePath -> ShakeOptions -> IO ())
  -> (FilePath -> BuildGraph -> Assertion)
  -> Assertion
withTelemetryBuild name buildFn checkFn =
  withSystemTempDirectory ("shake-thread-loss-" ++ name) $ \tmpDir -> do
    state <- newTelemetryState
    let opts =
          shakeOptions
            { shakeFiles = tmpDir </> ".shake"
            , shakeThreads = 8
            , shakeVerbosity = T.Silent
            , shakeExtra = T.addShakeExtra state (shakeExtra shakeOptions)
            }
    buildFn tmpDir opts
    graph <- freezeGraph state
    checkFn tmpDir (computeCriticalPath graph)

runBuild :: ShakeOptions -> T.Rules () -> IO ()
runBuild = Shake.shake

findNodeByLabel :: Text -> BuildGraph -> Maybe Node
findNodeByLabel label graph =
  let nodes = IntMap.elems (graphNodes graph)
   in case filter (\n -> nodeLabel n == label) nodes of
        (n : _) -> Just n
        [] -> Nothing

assertEdgeByLabel :: Text -> Text -> BuildGraph -> Assertion
assertEdgeByLabel fromLabel toLabel graph =
  case (findNodeByLabel fromLabel graph, findNodeByLabel toLabel graph) of
    (Nothing, _) -> assertFailure ("missing source node: " ++ Text.unpack fromLabel)
    (_, Nothing) -> assertFailure ("missing target node: " ++ Text.unpack toLabel)
    (Just fromNode, Just toNode) -> do
      let hasEdge = Vector.any
            (\e -> edgeFrom e == nodeId fromNode && edgeTo e == nodeId toNode)
            (graphEdges graph)
      assertBool
        ( "expected edge "
            ++ Text.unpack fromLabel
            ++ " -> "
            ++ Text.unpack toLabel
        )
        hasEdge

testNeedAfterSuspension :: Assertion
testNeedAfterSuspension =
  withTelemetryBuild "need-after-suspension" buildFn checkFn
  where
    buildFn tmpDir opts = runBuild opts $ do
      (tmpDir </> "slow-a.dep") T.%> \out -> do
        liftIO $ threadDelayMs 200
        T.writeFile' out "a"
      (tmpDir </> "b.dep") T.%> \out -> T.writeFile' out "b"
      (tmpDir </> "c.dep") T.%> \out -> T.writeFile' out "c"
      (tmpDir </> "d.dep") T.%> \out -> T.writeFile' out "d"
      (tmpDir </> "main.need") T.%> \out -> do
        T.need [tmpDir </> "slow-a.dep"]
        T.need [tmpDir </> "b.dep"]
        T.need [tmpDir </> "c.dep"]
        T.need [tmpDir </> "d.dep"]
        T.writeFile' out "main"
      T.want [tmpDir </> "main.need"]

    checkFn tmpDir graph = do
      let mainLabel = Text.pack (tmpDir </> "main.need")
      -- Expected failure: the first need usually blocks (dependency not ready),
      -- so apply/captureRAW suspends and resumes on another pool thread.
      -- The resumed thread has no ThreadId->NodeId entry, so later edges are dropped.
      assertEdgeByLabel mainLabel (Text.pack (tmpDir </> "slow-a.dep")) graph
      -- Expected to fail today (context lost after the first blocked need).
      assertEdgeByLabel mainLabel (Text.pack (tmpDir </> "b.dep")) graph
      -- Expected to fail today for the same reason.
      assertEdgeByLabel mainLabel (Text.pack (tmpDir </> "c.dep")) graph
      -- Expected to fail today for the same reason.
      assertEdgeByLabel mainLabel (Text.pack (tmpDir </> "d.dep")) graph

testAskOracleAfterSuspension :: Assertion
testAskOracleAfterSuspension =
  withTelemetryBuild "ask-oracle-after-suspension" buildFn checkFn
  where
    buildFn tmpDir opts = runBuild opts $ do
      _ <- T.addOracle $ \(ContextOracleQ q) -> do
        if q == "slow" then liftIO $ threadDelayMs 150 else pure ()
        pure ("value:" ++ q)
      (tmpDir </> "oracle.out") T.%> \out -> do
        _ <- T.askOracle (ContextOracleQ "slow")
        _ <- T.askOracle (ContextOracleQ "fast-1")
        _ <- T.askOracle (ContextOracleQ "fast-2")
        T.writeFile' out "done"
      T.want [tmpDir </> "oracle.out"]

    checkFn tmpDir graph = do
      let mainLabel = Text.pack (tmpDir </> "oracle.out")
      -- Expected failure: askOracle is apply-based; after the slow query blocks,
      -- continuation resume can happen on a different thread without context.
      assertEdgeByLabel mainLabel (Text.pack (show (ContextOracleQ "slow"))) graph
      -- Expected to fail today (edge after resume is silently skipped).
      assertEdgeByLabel mainLabel (Text.pack (show (ContextOracleQ "fast-1"))) graph
      -- Expected to fail today (edge after resume is silently skipped).
      assertEdgeByLabel mainLabel (Text.pack (show (ContextOracleQ "fast-2"))) graph

testParallelParentResume :: Assertion
testParallelParentResume =
  withTelemetryBuild "parallel-parent-resume" buildFn checkFn
  where
    buildFn tmpDir opts = runBuild opts $ do
      forM_ ["x.dep", "y.dep", "z.dep", "after-1.dep", "after-2.dep"] $ \f ->
        (tmpDir </> f) T.%> \out -> do
          if "after" `Text.isInfixOf` Text.pack f then pure () else liftIO $ threadDelayMs 120
          T.writeFile' out f
      (tmpDir </> "parallel.main") T.%> \out -> do
        _ <- T.parallel
          [ T.need [tmpDir </> "x.dep"]
          , T.need [tmpDir </> "y.dep"]
          , T.need [tmpDir </> "z.dep"]
          ]
        T.need [tmpDir </> "after-1.dep"]
        T.need [tmpDir </> "after-2.dep"]
        T.writeFile' out "ok"
      T.want [tmpDir </> "parallel.main"]

    checkFn tmpDir graph = do
      let mainLabel = Text.pack (tmpDir </> "parallel.main")
      assertEdgeByLabel mainLabel (Text.pack (tmpDir </> "x.dep")) graph
      assertEdgeByLabel mainLabel (Text.pack (tmpDir </> "y.dep")) graph
      assertEdgeByLabel mainLabel (Text.pack (tmpDir </> "z.dep")) graph
      -- Expected failure: children inherit context, but parent waits on a fence.
      -- After actionFenceRequeue resumes, parent may be on a thread with no context.
      assertEdgeByLabel mainLabel (Text.pack (tmpDir </> "after-1.dep")) graph
      -- Expected to fail today for the same parent-resume reason.
      assertEdgeByLabel mainLabel (Text.pack (tmpDir </> "after-2.dep")) graph

testForPParentResume :: Assertion
testForPParentResume =
  withTelemetryBuild "forp-parent-resume" buildFn checkFn
  where
    buildFn tmpDir opts = runBuild opts $ do
      forM_ ["a.dep", "b.dep", "c.dep", "after-forp.dep"] $ \f ->
        (tmpDir </> f) T.%> \out -> do
          if "after" `Text.isInfixOf` Text.pack f then pure () else liftIO $ threadDelayMs 100
          T.writeFile' out f
      (tmpDir </> "forp.main") T.%> \out -> do
        _ <- T.forP ["a.dep", "b.dep", "c.dep"] $ \dep ->
          T.need [tmpDir </> dep]
        T.need [tmpDir </> "after-forp.dep"]
        T.writeFile' out "ok"
      T.want [tmpDir </> "forp.main"]

    checkFn tmpDir graph = do
      let mainLabel = Text.pack (tmpDir </> "forp.main")
      assertEdgeByLabel mainLabel (Text.pack (tmpDir </> "a.dep")) graph
      assertEdgeByLabel mainLabel (Text.pack (tmpDir </> "b.dep")) graph
      assertEdgeByLabel mainLabel (Text.pack (tmpDir </> "c.dep")) graph
      -- Expected failure: forP has the same fence-based parent resumption behavior
      -- as parallel; post-forP dependency calls can lose source attribution.
      assertEdgeByLabel mainLabel (Text.pack (tmpDir </> "after-forp.dep")) graph

testParParentResume :: Assertion
testParParentResume =
  withTelemetryBuild "par-parent-resume" buildFn checkFn
  where
    buildFn tmpDir opts = runBuild opts $ do
      forM_ ["left.dep", "right.dep", "after-par.dep"] $ \f ->
        (tmpDir </> f) T.%> \out -> do
          if "after" `Text.isInfixOf` Text.pack f then pure () else liftIO $ threadDelayMs 110
          T.writeFile' out f
      (tmpDir </> "par.main") T.%> \out -> do
        _ <- T.par
          (T.need [tmpDir </> "left.dep"])
          (T.need [tmpDir </> "right.dep"])
        T.need [tmpDir </> "after-par.dep"]
        T.writeFile' out "ok"
      T.want [tmpDir </> "par.main"]

    checkFn tmpDir graph = do
      let mainLabel = Text.pack (tmpDir </> "par.main")
      assertEdgeByLabel mainLabel (Text.pack (tmpDir </> "left.dep")) graph
      assertEdgeByLabel mainLabel (Text.pack (tmpDir </> "right.dep")) graph
      -- Expected failure: par also suspends parent until both branches complete.
      -- On resume, parent can continue on a thread missing telemetry context.
      assertEdgeByLabel mainLabel (Text.pack (tmpDir </> "after-par.dep")) graph

testBatchItemResume :: Assertion
testBatchItemResume =
  withTelemetryBuild "batch-item-resume" buildFn checkFn
  where
    buildFn tmpDir opts = runBuild opts $ do
      (tmpDir </> "shared.dep") T.%> \out ->
        T.writeFile' out "shared"

      T.batch 2
        ((tmpDir </> "*.bat") T.%>)
        (\out -> pure out)
        (\outs -> do
          T.need [tmpDir </> "shared.dep"]
          mapM_ (\out -> T.writeFile' out "batched") outs
        )

      T.want [tmpDir </> "a.bat", tmpDir </> "b.bat"]

    checkFn tmpDir graph = do
      -- Expected model: dependencies observed while completing batched items
      -- should be attributed to the item node continuation.
      -- Expected failure today: once batch item actions suspend waiting for collect,
      -- resumed continuations do not retain per-item thread context.
      assertEdgeByLabel "batch-item" (Text.pack (tmpDir </> "shared.dep")) graph

threadDelayMs :: Int -> IO ()
threadDelayMs ms = threadDelay (ms * 1000)
