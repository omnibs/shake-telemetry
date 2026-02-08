{-# LANGUAGE OverloadedStrings #-}

module Test.Telemetry.MermaidTest (mermaidTests) where

import Data.IntMap.Strict qualified as IntMap
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime (..))
import Data.Time.Calendar (fromGregorian)
import Data.Vector qualified as Vector
import Test.Tasty
import Test.Tasty.HUnit

import Development.Shake.Telemetry.Graph
import Development.Shake.Telemetry.Mermaid

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

mermaidTests :: TestTree
mermaidTests =
  testGroup
    "Mermaid"
    [ testCase "renders critical path as graph LR" $ do
        let graph = mkGraph [(0, "compile", 2.5), (1, "link", 5.0)] [(1, 0)] [0, 1]
            output = renderMermaid graph
        assertBool "starts with graph LR" ("graph LR" `T.isPrefixOf` output)
        assertBool "has classDef" ("classDef critical" `T.isInfixOf` output)
        assertBool "has edge arrow" ("-->" `T.isInfixOf` output)
        assertBool "has class statement" ("class " `T.isInfixOf` output)
    , testCase "single node critical path" $ do
        let graph = mkGraph [(0, "only", 3.0)] [] [0]
            output = renderMermaid graph
        assertBool "has node label" ("only" `T.isInfixOf` output)
        assertBool "has duration" ("3.0s" `T.isInfixOf` output)
    , testCase "empty critical path produces minimal output" $ do
        let graph = mkGraph [(0, "a", 1.0)] [] []
            output = renderMermaid graph
        assertBool "starts with graph LR" ("graph LR" `T.isPrefixOf` output)
    ]
