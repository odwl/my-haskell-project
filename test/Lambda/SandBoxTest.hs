{-# LANGUAGE ScopedTypeVariables #-}

module Lambda.SandBoxTest (sandBoxSuite) where

import Lambda.SandBox (halve, third, third', third'')
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
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
        "third"
        [ testCase "gets the third element of an Int list" $
            third [1, 2, 3, 4] @?= (3 :: Int),
          testCase "gets the third element of a String list" $
            third ["a", "b", "c", "d"] @?= "c",
          testProperty "matches list indexing at 2" $
            \(xs :: [Int]) ->
              (length xs >= 3) ==> third xs == xs !! 2
        ],
      testGroup
        "third'"
        [ testCase "gets the third element of an Int list" $
            third' [1, 2, 3, 4] @?= (3 :: Int),
          testCase "gets the third element of a String list" $
            third' ["a", "b", "c", "d"] @?= "c",
          testProperty "matches list indexing at 2" $
            \(xs :: [Int]) ->
              (length xs >= 3) ==> third' xs == xs !! 2,
          testProperty "is equivalent to third" $
            \(xs :: [Int]) ->
              (length xs >= 3) ==> third' xs == third xs
        ],
      testGroup
        "third''"
        [ testCase "gets the third element of an Int list" $
            third'' [1, 2, 3, 4] @?= (3 :: Int),
          testProperty "is equivalent to third" $
            \(xs :: [Int]) ->
              (length xs >= 3) ==> third'' xs == third xs
        ]
    ]
