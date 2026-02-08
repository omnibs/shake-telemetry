module Main where

import Test.Tasty

import Test.Telemetry.StateTest (stateTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "shake-telemetry"
    [ stateTests
    ]
