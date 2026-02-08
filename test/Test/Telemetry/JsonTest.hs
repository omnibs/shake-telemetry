{-# LANGUAGE OverloadedStrings #-}

module Test.Telemetry.JsonTest (jsonTests) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.IntMap.Strict qualified as IntMap
import Data.Text (Text)
import Data.Time (UTCTime (..))
import Data.Time.Calendar (fromGregorian)
import Data.Vector qualified as Vector
import Test.Tasty
import Test.Tasty.HUnit

import Development.Shake.Telemetry.Graph
import Development.Shake.Telemetry.Json ()

mkGraph :: [(Int, Text, Double)] -> [(Int, Int)] -> [Int] -> BuildGraph
mkGraph nodeSpecs edgeSpecs cp =
  BuildGraph
    { graphNodes = IntMap.fromList [(nid, mkNode nid lbl dur) | (nid, lbl, dur) <- nodeSpecs]
    , graphEdges = Vector.fromList [Edge from to | (from, to) <- edgeSpecs]
    , graphBuildStart = UTCTime (fromGregorian 2024 1 15) 37800
    , graphTotalSeconds = 120.5
    , graphCriticalPath = cp
    }
  where
    mkNode nid lbl dur = Node nid lbl FileNode (Just 0) (Just dur) (Just dur)

jsonTests :: TestTree
jsonTests =
  testGroup
    "Json"
    [ testCase "round-trip: encode then decode equals original" $ do
        let graph = mkGraph [(0, "a.o", 1.0), (1, "b.o", 2.0)] [(1, 0)] [0, 1]
            encoded = Aeson.encode graph
        case Aeson.eitherDecode encoded of
          Left err -> assertFailure ("decode failed: " ++ err)
          Right decoded -> assertEqual "round-trip" graph decoded
    , testCase "node with no timing data round-trips with nulls" $ do
        let node = Node 0 "placeholder" FileNode Nothing Nothing Nothing
            graph =
              BuildGraph
                { graphNodes = IntMap.singleton 0 node
                , graphEdges = Vector.empty
                , graphBuildStart = UTCTime (fromGregorian 2024 1 1) 0
                , graphTotalSeconds = 0
                , graphCriticalPath = []
                }
            encoded = Aeson.encode graph
        case Aeson.eitherDecode encoded of
          Left err -> assertFailure ("decode failed: " ++ err)
          Right decoded -> assertEqual "round-trip with nulls" graph decoded
    , testCase "criticalPath field present in JSON" $ do
        let graph = mkGraph [(0, "x", 1.0)] [] [0]
            val = Aeson.toJSON graph
        case val of
          Aeson.Object obj ->
            assertBool "has criticalPath key" $
              KeyMap.member (Key.fromString "criticalPath") obj
          _ -> assertFailure "expected JSON object"
    , testCase "onCriticalPath set correctly on nodes" $ do
        let graph = mkGraph [(0, "a", 1.0), (1, "b", 2.0), (2, "c", 3.0)] [(1, 0), (2, 1)] [0, 1]
            val = Aeson.toJSON graph
        case val of
          Aeson.Object obj -> case KeyMap.lookup (Key.fromString "nodes") obj of
            Just (Aeson.Array nodes) -> do
              let checkNode v = case v of
                    Aeson.Object n ->
                      case ( KeyMap.lookup (Key.fromString "id") n
                           , KeyMap.lookup (Key.fromString "onCriticalPath") n
                           ) of
                        (Just (Aeson.Number nid), Just (Aeson.Bool onCP)) -> Just (round nid :: Int, onCP)
                        _ -> Nothing
                    _ -> Nothing
                  nodeFlags = map checkNode (Vector.toList nodes)
              assertBool "node 0 on CP" $ Just (0, True) `elem` nodeFlags
              assertBool "node 1 on CP" $ Just (1, True) `elem` nodeFlags
              assertBool "node 2 not on CP" $ Just (2, False) `elem` nodeFlags
            _ -> assertFailure "expected nodes array"
          _ -> assertFailure "expected JSON object"
    ]
