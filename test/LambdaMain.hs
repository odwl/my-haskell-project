module Main where

import Lambda.DataKindsTest (dataKindsTests)
import Lambda.RequiredTypeArgumentsTest (requiredTypeArgumentsTests)
import LambdaTest (lambdaSuite)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "Lambda Suite" [lambdaSuite, dataKindsTests, requiredTypeArgumentsTests]
