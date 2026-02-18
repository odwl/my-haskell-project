module LambdaTest where

import Test.HUnit
import System.Exit (exitFailure, exitSuccess)
import Data.Function (fix) -- The standard library 'y'

testSimpleSquare :: Test 
testSimpleSquare = TestCase $ assertEqual "should square 2" 4 (square 2)
  where
    square = \x -> x^2
    
testDoubleApplication :: Test 
testDoubleApplication = TestCase  $ assertEqual "should give 16 " 16 (applyTwice square 2)
  where 
    applyTwice = \f -> (\x -> f (f x))
    square x = x^2
    

testYFact :: Test 
testYFact = TestCase $ assertEqual "Should be 6" 6 (fact 3)
  where 
    step f n = if n == 0 then 1 else n * f (n - 1)
    fact = fix step

tests :: Test
tests = TestList 
    [ TestLabel "Simple Square"    testSimpleSquare
    , TestLabel "Double Apply"     testDoubleApplication
    , TestLabel "YFactorial"       testYFact
    ]

main :: IO ()
main = do
    putStrLn "\n--- Running: Lambda Suite ---"
    counts <- runTestTT tests
    if errors counts + failures counts == 0
        then exitSuccess
        else exitFailure