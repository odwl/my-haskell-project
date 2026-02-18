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

(-->) :: (Show a, Eq a) => a -> a -> Test
got --> expected = TestCase $ assertEqual "" expected got

yFactTests :: Test 
yFactTests = TestLabel "YFact" $ TestList
    [ fact 0 --> 1
    , fact 1 --> 1
    , fact 2 --> 2
    , fact 3 --> 6
    ]

safeHeadTests :: Test
safeHeadTests = TestLabel "SafeHead" $ TestList 
    [ safeHead ([] :: [Int]) --> Nothing
    , safeHead [1,2,3] --> Just 1
    , safeHead [10] --> Just 10 
    ]

addMaybesTests :: Test 
addMaybesTests = TestLabel "AddMaybe" $ TestList
    [ addMaybes (Just 1) (Just 1) --> Just 2
    , addMaybes Nothing  (Just 1) --> Nothing
    , addMaybes (Just 1) Nothing  --> Nothing
    , addMaybes Nothing  Nothing  --> Nothing
    ]

tests :: Test
tests = TestList
    [ TestLabel "Simple Square"    testSimpleSquare
    , TestLabel "Double Apply"     testDoubleApplication
    , yFactTests
    , safeHeadTests
    , addMaybesTests
    ]

main :: IO ()
main = do
    putStrLn "\n--- Running: Lambda Suite ---"
    counts <- runTestTT tests
    if errors counts + failures counts == 0
        then exitSuccess
        else exitFailure
