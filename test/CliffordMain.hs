module Main (main) where

import Test.Tasty
import Lambda.MultiVectorTest (multiVectorTests)
import Lambda.Clifford.UniversalTest (universalCliffordTests)
import Lambda.Clifford.VersorTest (versorTests)

main :: IO ()
main = defaultMain $ testGroup "Geometric & Universal Clifford Algebra Tests"
  [ multiVectorTests
  , universalCliffordTests
  , versorTests
  ]
