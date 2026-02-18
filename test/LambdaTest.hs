module Main where

import Test.HUnit
import System.Exit (exitFailure, exitSuccess)
import Lambda

applyTwice = \f -> (\x -> f (f x))
square :: Int -> Int
square x = x^2

testSimpleSquare :: Test
testSimpleSquare = TestLabel "Simple Square" $ TestList
    [ square 2 --> 4 ]

testDoubleApplication :: Test
testDoubleApplication = TestLabel "Double Apply" $ TestList
    [ applyTwice square 2 --> 16
    , applyTwice (+1) 10  --> 12
    ]

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
    [ testSimpleSquare
    , testDoubleApplication
    [ testDoubleApplication
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
