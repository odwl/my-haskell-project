module Main where

import Exercism.AnagramTest (anagramTests)
import Exercism.BobTest (bobTests)
import Exercism.PangramTest (pangramTests)
import Exercism.ReverseStringTest (reverseStringTests)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup
    "Exercism"
    [ anagramTests,
      reverseStringTests,
      pangramTests,
      bobTests
    ]

main :: IO ()
main = do
  putStrLn "\n--- Running: Exercism Suite ---"
  defaultMain tests
