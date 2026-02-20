module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Exercism.ReverseStringTest (reverseStringTests)
import Exercism.PangramTest (pangramTests)

tests :: TestTree
tests = testGroup "Exercism"
    [ reverseStringTests
    , pangramTests
    ]

main :: IO ()
main = do
    putStrLn "\n--- Running: Exercism Suite ---"
    defaultMain tests
