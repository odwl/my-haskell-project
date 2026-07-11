module Main where

import Lambda.DataKindsTest (dataKindsTests)
import LambdaTest (lambdaSuite)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "Lambda Suite" [lambdaSuite, dataKindsTests]
