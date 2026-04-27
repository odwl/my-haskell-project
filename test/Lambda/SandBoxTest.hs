{-# LANGUAGE ScopedTypeVariables #-}

module Lambda.SandBoxTest (sandBoxSuite) where

import Lambda.SandBox (halve, third, third', sTail, sTail', sTail'')
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, (==>))

sandBoxSuite :: TestTree
sandBoxSuite =
  testGroup
    "SandBox Tests"
    [ testGroup
        "halve (splitAt)"
        [ testCase "returns Nothing for odd length list" $
            halve ([1, 2, 3] :: [Int]) @?= Nothing,
          testProperty "splits an even length list into two equal length halves" $
            \(xs :: [Int]) ->
              even (length xs) ==>
                case halve xs of
                  Just (l, r) -> length l == length r
                  Nothing -> False,
          testProperty "preserves all elements" $
            \(xs :: [Int]) ->
              even (length xs) ==>
                case halve xs of
                  Just (l, r) -> l ++ r == xs
                  Nothing -> False
        ],
      testGroup
        "third"
        [ testCase "gets the third element of an Int list" $
            third [1, 2, 3, 4] @?= Just (3 :: Int),
          testCase "returns Nothing for short list" $
            third ([1, 2] :: [Int]) @?= Nothing,
          testProperty "matches list indexing at 2" $
            \(xs :: [Int]) ->
              (length xs >= 3) ==> third xs == Just (xs !! 2)
        ],
      testGroup
        "third'"
        [ testCase "gets the third element of an Int list" $
            third' [1, 2, 3, 4] @?= Just (3 :: Int),
          testCase "returns Nothing for short list" $
            third' ([1, 2] :: [Int]) @?= Nothing,
          testProperty "matches list indexing at 2" $
            \(xs :: [Int]) ->
              (length xs >= 3) ==> third' xs == Just (xs !! 2),
          testProperty "is equivalent to third" $
            \(xs :: [Int]) ->
              (length xs >= 3) ==> third' xs == third xs
        ],
      testGroup
        "sTail"
        [ testCase "returns empty list for empty list" $
            sTail ([] :: [Int]) @?= [],
          testCase "returns tail for non-empty list" $
            sTail ([1, 2, 3] :: [Int]) @?= [2, 3],
          testProperty "is equivalent to tail for non-empty lists" $
            \(xs :: [Int]) ->
              not (null xs) ==> sTail xs == tail xs,
          testProperty "is equivalent to sTail'" $
            \(xs :: [Int]) ->
              sTail xs == sTail' xs,
          testProperty "is equivalent to sTail''" $
            \(xs :: [Int]) ->
              sTail xs == sTail'' xs
        ]
    ]
