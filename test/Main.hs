module Main where

import ExercismTest (exercismSuite)
import Lambda.DataKindsTest (dataKindsTests)
import Lambda.RequiredTypeArgumentsTest (requiredTypeArgumentsTests)
import Lambda.SandBoxTest (sandBoxSuite)
import LambdaTest (lambdaSuite)
import CountDownTest (countDownTests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "All Tests" [lambdaSuite, exercismSuite, sandBoxSuite, dataKindsTests, requiredTypeArgumentsTests, countDownTests]
