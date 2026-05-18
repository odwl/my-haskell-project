{-# LANGUAGE ScopedTypeVariables #-}

module Lambda.SandBoxTest (sandBoxSuite) where

import Control.Arrow (Arrow (..), ArrowChoice (..), ArrowZero (..), (>>>))
import Control.Category ((.), id)
import Data.Profunctor (Profunctor (..))
import Prelude hiding (id, (.))
import Lambda.SandBox (WriterKleisli (..), halve, nt, nt2, nt3, sTail, sTail', sTail'', third, third')
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, (==>))

f :: Int -> Int
f = (+ 1)

g :: Int -> Int
g = (* 2)



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
        "WriterKleisli Arrow Laws"
        [ testProperty "Category Identity: id . f == f" $
            \(w :: String, x :: Int) ->
              runWriterKleisli (id . WriterKleisli mF) (w, x) == runWriterKleisli (WriterKleisli mF) (w, x),
          testProperty "Category Composition: (f . g) . h == f . (g . h)" $
            \(w :: String, x :: Int) ->
              runWriterKleisli ((WriterKleisli mF . WriterKleisli mG) . WriterKleisli mH) (w, x) == runWriterKleisli (WriterKleisli mF . (WriterKleisli mG . WriterKleisli mH)) (w, x),
          testProperty "Arrow law 1: arr id == id" $
            \(w :: String, x :: Int) ->
              runWriterKleisli (arr id :: WriterKleisli String Maybe Int Int) (w, x) == runWriterKleisli id (w, x),
          testProperty "Arrow law 2: arr (f . g) == arr f . arr g" $
            \(w :: String, x :: Int) ->
              runWriterKleisli (arr (f . g) :: WriterKleisli String Maybe Int Int) (w, x) == runWriterKleisli (arr f . arr g) (w, x),
          testProperty "Arrow law 3: first (arr f) == arr (first f)" $
            \(w :: String, x :: Int, d :: Char) ->
              runWriterKleisli (first (arr f :: WriterKleisli String Maybe Int Int)) (w, (x, d)) == runWriterKleisli (arr (first f)) (w, (x, d)),
          testProperty "ArrowZero law: zeroArrow >>> f == zeroArrow" $
            \(w :: String, x :: Int) ->
              runWriterKleisli (zeroArrow >>> WriterKleisli mF :: WriterKleisli String Maybe Int Int) (w, x) == runWriterKleisli zeroArrow (w, x),
          testProperty "ArrowChoice law: left (arr f) == arr (left f)" $
            \(w :: String, e :: Either Int Char) ->
              runWriterKleisli (left (arr f :: WriterKleisli String Maybe Int Int)) (w, e) == runWriterKleisli (arr (left f)) (w, e)
        ],
      testGroup
        "WriterKleisli Profunctor Laws"
        [ testProperty "Profunctor Identity: dimap id id == id" $
            \(w :: String, x :: Int) ->
              runWriterKleisli (dimap id id (WriterKleisli mF)) (w, x) == runWriterKleisli (WriterKleisli mF) (w, x),
          testProperty "Profunctor Composition: dimap (f . g) (h . i) == dimap g h . dimap f i" $
            \(w :: String, x :: Int) ->
              let l1 = (+ 1)
                  l2 = (* 2)
                  r1 = (+ 5)
                  r2 = (* 3)
               in runWriterKleisli (dimap (l2 . l1) (r1 . r2) (WriterKleisli mF)) (w, x) == runWriterKleisli (dimap l1 r1 (dimap l2 r2 (WriterKleisli mF))) (w, x)
        ],
      testGroup
        "Natural Transformation Laws"
        [ testProperty "Naturality of nt (1 element): fmap f . nt == nt . fmap f" $
            \(m :: Maybe Int) ->
              (fmap f . nt) m == (nt . fmap f) m,
          testProperty "Naturality of nt2 (0 elements): fmap f . nt2 == nt2 . fmap f" $
            \(m :: Maybe Int) ->
              (fmap f . nt2) m == (nt2 . fmap f) m,
          testProperty "Naturality of nt3 (2 elements): fmap f . nt3 == nt3 . fmap f" $
            \(m :: Maybe Int) ->
              (fmap f . nt3) m == (nt3 . fmap f) m
        ]
    ]
