module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Exercism.ReverseStringTest (reverseStringTests)

tests :: TestTree
tests = testGroup "Exercism"
    [ reverseStringTests
    ]

main :: IO ()
main = do
    putStrLn "\n--- Running: Exercism Suite ---"
    defaultMain tests
