module Main where

import LambdaTest (lambdaSuite)
import Test.Tasty (defaultMain)

main :: IO ()
main = defaultMain lambdaSuite
