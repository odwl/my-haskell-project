module Main where

import Exercism.BobTest (bobTests)
import Exercism.PangramTest (pangramTests)
import Exercism.ReverseStringTest (reverseStringTests)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup
    "Exercism"
    [ reverseStringTests,
      pangramTests,
      bobTests
    ]

main :: IO ()
main = do
  putStrLn "\n--- Running: Exercism Suite ---"
  defaultMain tests
