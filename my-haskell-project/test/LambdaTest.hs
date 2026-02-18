module LambdaTest where

import Test.HUnit
import System.Exit (exitFailure, exitSuccess)

test2 :: Test 
test2 = TestCase (assertEqual "should square 2" 4 ((\a -> a^2) 2))

main :: IO ()
main = do
    counts <- runTestTT test2
    if errors counts + failures counts == 0
        then exitSuccess
        else exitFailure