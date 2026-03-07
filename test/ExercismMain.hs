module Main where

import ExercismTest (exercismSuite)
import Test.Tasty (defaultMain)

main :: IO ()
main = defaultMain exercismSuite
