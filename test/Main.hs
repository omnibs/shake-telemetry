module Main where

import Test.Tasty

import Test.Telemetry.CriticalPathTest (criticalPathTests)
import Test.Telemetry.StateTest (stateTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "shake-telemetry"
    [ stateTests
    , criticalPathTests
    ]
