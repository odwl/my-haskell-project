module Main where

import ExercismTest (exercismSuite)
import LambdaTest (lambdaSuite)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  putStrLn "DEBUG: Running all tests..."
  defaultMain $ testGroup "All Tests" [lambdaSuite, exercismSuite]
