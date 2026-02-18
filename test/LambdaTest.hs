module LambdaTest where

import Test.HUnit
import System.Exit (exitFailure, exitSuccess)

testSimpleSquare :: Test 
testSimpleSquare = TestCase $ do
    let square = \x -> x^2
    assertEqual "should square 2" 4 (square 2)

testDoubleApplication :: Test 
  
testDoubleApplication = TestCase  $ do 
    let applyTwice = \f -> (\x -> f (f x))
    let square = \x -> x^2
    assertEqual "should give 16 " 16 (applyTwice square 2)

tests :: Test
tests = TestList 
    [ TestLabel "Simple Square"    testSimpleSquare
    , TestLabel "Double Apply"     testDoubleApplication
    ]

main :: IO ()
main = do
    putStrLn "\n--- Running: Lambda Suite ---"
    counts <- runTestTT tests
    if errors counts + failures counts == 0
        then exitSuccess
        else exitFailure