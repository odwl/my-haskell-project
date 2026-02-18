module Main where

import Test.HUnit
import System.Exit (exitFailure, exitSuccess)
import Lambda

testSimpleSquare :: Test
testSimpleSquare = TestCase $ assertEqual "should square 2" 4 (square 2)
  where
    square = \x -> x^2

testDoubleApplication :: Test
testDoubleApplication = TestCase  $ assertEqual "should give 16 " 16 (applyTwice square 2)
  where
    applyTwice = \f -> (\x -> f (f x))
    square x = x^2

yFactTests :: Test 
yFactTests = TestLabel "YFact" $ TestList
    [ TestCase $ assertEqual "fact 0" 1 (fact 0)
    , TestCase $ assertEqual "fact 1" 1 (fact 1)
    , TestCase $ assertEqual "fact 2" 2 (fact 2)
    , TestCase $ assertEqual "fact 3" 6 (fact 3)
    ]

safeHeadTests :: Test
safeHeadTests = TestLabel "SafeHead Parametrized" $ TestList 
    [ TestCase (assertEqual msg expected (safeHead input)) 
    | (msg, expected, input) <- cases 
    ]
  where
    cases = 
        [ ("Empty List", Nothing,       [] :: [Int])
        , ("Populated",  Just 1,        [1, 2, 3])
        , ("Single",     Just 10,       [10])
        ]

tests :: Test
tests = TestList
    [ TestLabel "Simple Square"    testSimpleSquare
    , TestLabel "Double Apply"     testDoubleApplication
    , yFactTests
    , safeHeadTests
    ]

main :: IO ()
main = do
    putStrLn "\n--- Running: Lambda Suite ---"
    counts <- runTestTT tests
    if errors counts + failures counts == 0
        then exitSuccess
        else exitFailure
