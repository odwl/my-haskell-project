module Main where

import Test.HUnit
import HelloWorld (sayHello)
import System.Exit (exitFailure, exitSuccess)

-- Define the test
test1 :: Test
test1 = TestCase (assertEqual "Should say Hello to Userd" "Hello, User!" (sayHello "User"))

-- Run the test
main :: IO ()
main = do
    counts <- runTestTT test1
    if errors counts + failures counts == 0
        then exitSuccess
        else exitFailure