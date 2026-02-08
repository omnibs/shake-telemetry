{-# LANGUAGE OverloadedStrings #-}

module Test.Telemetry.StateTest (stateTests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM, forM_, replicateM_)
import Data.IntMap.Strict qualified as IntMap
import Data.Text qualified as T
import Data.Vector qualified as Vector
import Test.Tasty
import Test.Tasty.HUnit

import Development.Shake.Telemetry.Graph
import Development.Shake.Telemetry.State

stateTests :: TestTree
stateTests =
  testGroup
    "State"
    [ testGroup "Basic operations" basicTests
    , testGroup "Node deduplication" dedupTests
    , testGroup "Concurrency" concurrencyTests
    ]

basicTests :: [TestTree]
basicTests =
  [ testCase "register and freeze 3 nodes with edges" $ do
      state <- newTelemetryState
      n1 <- registerNode state "a.o" FileNode
      n2 <- registerNode state "b.o" FileNode
      n3 <- registerNode state "main" PhonyNode
      recordEdge state n3 "a.o" FileNode
      recordEdge state n3 "b.o" FileNode
      finishNode state n1
      finishNode state n2
      finishNode state n3
      graph <- freezeGraph state
      assertEqual "node count" 3 (IntMap.size (graphNodes graph))
      assertEqual "edge count" 2 (Vector.length (graphEdges graph))
      -- Verify labels
      let labels = map nodeLabel (IntMap.elems (graphNodes graph))
      assertBool "has a.o" ("a.o" `elem` labels)
      assertBool "has b.o" ("b.o" `elem` labels)
      assertBool "has main" ("main" `elem` labels)
      -- Verify types
      let node3 = graphNodes graph IntMap.! n3
      assertEqual "phony type" PhonyNode (nodeType node3)
  , testCase "register and finish gives non-negative duration" $ do
      state <- newTelemetryState
      nid <- registerNode state "target" FileNode
      finishNode state nid
      graph <- freezeGraph state
      let node = graphNodes graph IntMap.! nid
      case nodeDuration node of
        Nothing -> assertFailure "expected duration"
        Just d -> assertBool ("duration non-negative: " ++ show d) (d >= 0)
  , testCase "different labels get different IDs" $ do
      state <- newTelemetryState
      n1 <- registerNode state "foo" FileNode
      n2 <- registerNode state "bar" FileNode
      assertBool "different IDs" (n1 /= n2)
  , testCase "thread context round-trip" $ do
      state <- newTelemetryState
      nid <- registerNode state "ctx-test" FileNode
      setThreadNode state nid
      got <- getThreadNode state
      assertEqual "thread node" (Just nid) got
  , testCase "getThreadNode returns Nothing without setThreadNode" $ do
      state <- newTelemetryState
      got <- getThreadNode state
      assertEqual "no thread node" Nothing got
  ]

dedupTests :: [TestTree]
dedupTests =
  [ testCase "getOrCreateNode twice returns same ID" $ do
      state <- newTelemetryState
      id1 <- getOrCreateNode state "shared" FileNode
      id2 <- getOrCreateNode state "shared" FileNode
      assertEqual "same ID" id1 id2
  , testCase "recordEdge then registerNode deduplicates" $ do
      state <- newTelemetryState
      src <- registerNode state "src" FileNode
      recordEdge state src "target" FileNode
      targetId <- registerNode state "target" FileNode
      graph <- freezeGraph state
      let edges = Vector.toList (graphEdges graph)
      let targetEdges = filter (\e -> edgeFrom e == src) edges
      assertEqual "one edge from src" 1 (length targetEdges)
      case targetEdges of
        [e] -> assertEqual "edge points to target" targetId (edgeTo e)
        _ -> assertFailure "expected exactly one edge"
  , testCase "registerNode then recordEdge deduplicates" $ do
      state <- newTelemetryState
      targetId <- registerNode state "target" FileNode
      src <- registerNode state "src" FileNode
      recordEdge state src "target" FileNode
      graph <- freezeGraph state
      let edges = Vector.toList (graphEdges graph)
      let targetEdges = filter (\e -> edgeFrom e == src) edges
      assertEqual "one edge from src" 1 (length targetEdges)
      case targetEdges of
        [e] -> assertEqual "edge points to target" targetId (edgeTo e)
        _ -> assertFailure "expected exactly one edge"
  ]

concurrencyTests :: [TestTree]
concurrencyTests =
  [ testCase "N threads registering unique nodes" $ do
      let n = 100
      state <- newTelemetryState
      dones <- forM [1 .. n] $ \i -> do
        done <- newEmptyMVar
        _ <- forkIO $ do
          _ <- registerNode state (T.pack $ "node-" ++ show i) FileNode
          putMVar done ()
        pure done
      forM_ dones takeMVar
      graph <- freezeGraph state
      assertEqual "node count" n (IntMap.size (graphNodes graph))
  , testCase "N threads recording edges to shared target" $ do
      let n = 50
      state <- newTelemetryState
      dones <- forM [1 .. n] $ \i -> do
        done <- newEmptyMVar
        _ <- forkIO $ do
          src <- registerNode state (T.pack $ "src-" ++ show i) FileNode
          recordEdge state src "shared-target" FileNode
          putMVar done ()
        pure done
      forM_ dones takeMVar
      graph <- freezeGraph state
      -- shared-target should exist once (dedup)
      let targetNodes =
            filter (\nd -> nodeLabel nd == "shared-target") (IntMap.elems (graphNodes graph))
      assertEqual "shared target exists once" 1 (length targetNodes)
      -- All edges should point to it
      targetId <- case targetNodes of
        [nd] -> pure (nodeId nd)
        _ -> assertFailure "expected exactly one target node" >> error "unreachable"
      let edgesToTarget =
            filter (\e -> edgeTo e == targetId) (Vector.toList (graphEdges graph))
      assertEqual "all edges to shared target" n (length edgesToTarget)
  ]
