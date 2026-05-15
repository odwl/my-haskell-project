{-# LANGUAGE ScopedTypeVariables #-}

module Lambda.SandBoxTest (sandBoxSuite) where

import Control.Arrow (Arrow (..), ArrowChoice (..), ArrowZero (..), (>>>))
import qualified Control.Category as C
import Lambda.SandBox (Writer (..), WriterKleisli (..), halve, sTail, sTail', sTail'', third, third')
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, (==>))

f :: Int -> Int
f = (+ 1)

g :: Int -> Int
g = (* 2)

hF :: (String, Int) -> (String, Int)
hF (w, x) = (w ++ "A", x + 1)

hG :: (String, Int) -> (String, Int)
hG (w, x) = (w ++ "B", x * 2)

hH :: (String, Int) -> (String, Int)
hH (w, x) = (w ++ "C", x - 3)

mF :: (String, Int) -> Maybe (String, Int)
mF (w, x) = Just (w ++ "A", x + 1)

mG :: (String, Int) -> Maybe (String, Int)
mG (w, x) = Just (w ++ "B", x * 2)

mH :: (String, Int) -> Maybe (String, Int)
mH (w, x) = Just (w ++ "C", x - 3)

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
        ],
      testGroup
        "Writer Arrow Laws"
        [ testProperty "Category Identity: id . f == f" $
            \(w :: String, x :: Int) ->
              runWriter (C.id C.. Writer hF) (w, x) == runWriter (Writer hF) (w, x),
          testProperty "Category Composition: (f . g) . h == f . (g . h)" $
            \(w :: String, x :: Int) ->
              runWriter ((Writer hF C.. Writer hG) C.. Writer hH) (w, x) == runWriter (Writer hF C.. (Writer hG C.. Writer hH)) (w, x),
          testProperty "Arrow law 1: arr id == id" $
            \(w :: String, x :: Int) ->
              runWriter (arr id) (w, x) == runWriter C.id (w, x),
          testProperty "Arrow law 2: arr (f . g) == arr f . arr g" $
            \(w :: String, x :: Int) ->
              runWriter (arr (f . g)) (w, x) == runWriter (arr f C.. arr g) (w, x),
          testProperty "Arrow law 3: first (arr f) == arr (first f)" $
            \(w :: String, x :: Int, d :: Char) ->
              runWriter (first (arr f)) (w, (x, d)) == runWriter (arr (first f)) (w, (x, d)),
          testProperty "ArrowChoice law: left (arr f) == arr (left f)" $
            \(w :: String, e :: Either Int Char) ->
              runWriter (left (arr f)) (w, e) == runWriter (arr (left f)) (w, e)
        ],
      testGroup
        "WriterKleisli Arrow Laws"
        [ testProperty "Category Identity: id . f == f" $
            \(w :: String, x :: Int) ->
              runWriterKleisli (C.id C.. WriterKleisli mF) (w, x) == runWriterKleisli (WriterKleisli mF) (w, x),
          testProperty "Category Composition: (f . g) . h == f . (g . h)" $
            \(w :: String, x :: Int) ->
              runWriterKleisli ((WriterKleisli mF C.. WriterKleisli mG) C.. WriterKleisli mH) (w, x) == runWriterKleisli (WriterKleisli mF C.. (WriterKleisli mG C.. WriterKleisli mH)) (w, x),
          testProperty "Arrow law 1: arr id == id" $
            \(w :: String, x :: Int) ->
              runWriterKleisli (arr id :: WriterKleisli String Maybe Int Int) (w, x) == runWriterKleisli C.id (w, x),
          testProperty "Arrow law 2: arr (f . g) == arr f . arr g" $
            \(w :: String, x :: Int) ->
              runWriterKleisli (arr (f . g) :: WriterKleisli String Maybe Int Int) (w, x) == runWriterKleisli (arr f C.. arr g) (w, x),
          testProperty "Arrow law 3: first (arr f) == arr (first f)" $
            \(w :: String, x :: Int, d :: Char) ->
              runWriterKleisli (first (arr f :: WriterKleisli String Maybe Int Int)) (w, (x, d)) == runWriterKleisli (arr (first f)) (w, (x, d)),
          testProperty "ArrowZero law: zeroArrow >>> f == zeroArrow" $
            \(w :: String, x :: Int) ->
              runWriterKleisli (zeroArrow >>> WriterKleisli mF :: WriterKleisli String Maybe Int Int) (w, x) == runWriterKleisli zeroArrow (w, x)
        ]
    ]
