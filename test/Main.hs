module Main where

import Test.Tasty

import Test.Telemetry.CriticalPathProps (criticalPathProps)
import Test.Telemetry.CriticalPathTest (criticalPathTests)
import Test.Telemetry.IntegrationTest (integrationTests)
import Test.Telemetry.JsonTest (jsonTests)
import Test.Telemetry.MermaidTest (mermaidTests)
import Test.Telemetry.ParityTest (parityTests)
import Test.Telemetry.StateTest (stateTests)
import Test.Telemetry.WrapTest (wrapTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "shake-telemetry"
    [ stateTests
    , criticalPathTests
    , criticalPathProps
    , jsonTests
    , mermaidTests
    , wrapTests
    , parityTests
    , integrationTests
    ]
