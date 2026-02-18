module LambdaTest where

import Test.HUnit
import System.Exit (exitFailure, exitSuccess)

test2 :: Test 
test2 = TestCase (assertEqual "should square 2" 4 ((\a -> a^2) 2))

testSquare :: Test 
testSquare = TestCase (assertEqual "should give 16 " 16 ((\f -> (\x -> f (f x))) (\a -> a^2) 2))

tests :: Test
tests = TestList [test2, testSquare]

main :: IO ()
main = do
    putStrLn "\n--- Running: Lambda Suite ---"
    counts <- runTestTT tests
    if errors counts + failures counts == 0
        then exitSuccess
        else exitFailure