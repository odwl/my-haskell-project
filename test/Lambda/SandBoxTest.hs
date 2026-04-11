{-# LANGUAGE ScopedTypeVariables #-}

module Lambda.SandBoxTest (sandBoxSuite) where

import Data.List (sort)
import Lambda.SandBox (halve, halve', halve'')
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, (==>))

sandBoxSuite :: TestTree
sandBoxSuite =
  testGroup
    "SandBox Tests"
    [ testGroup
        "halve (splitAt)"
        [ testProperty "splits an even length list into two equal length halves" $
            \(xs :: [Int]) ->
              (length xs `mod` 2 == 0) ==>
                let (l, r) = halve xs
                 in length l == length r,
          testProperty "preserves all elements" $
            \(xs :: [Int]) ->
              (length xs `mod` 2 == 0) ==>
                let (l, r) = halve xs
                 in l ++ r == xs
        ],
      testGroup
        "halve' vs halve''"
        [ testProperty "halve' is NOT the same as halve'' for N > 2 (EXPECTED TO FAIL)" $
            \(xs :: [Int]) ->
              (length xs `mod` 2 == 0) ==>
                halve' xs == halve'' xs
        ],
      testGroup
        "halve' (Alternating)"
        [ testProperty "splits an even length list into two equal length halves" $
            \(xs :: [Int]) ->
              (length xs `mod` 2 == 0) ==>
                let (l, r) = halve' xs
                 in length l == length r,
          testProperty "preserves all elements (ignoring order)" $
            \(xs :: [Int]) ->
              (length xs `mod` 2 == 0) ==>
                let (l, r) = halve' xs
                 in sort (l ++ r) == sort xs
        ],
      testGroup
        "halve'' (Outer-Inner)"
        [ testProperty "matches splitAt result" $
            \(xs :: [Int]) ->
              (length xs `mod` 2 == 0) ==>
                halve'' xs == halve xs
        ]
    ]
