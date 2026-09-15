{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lambda.StreamTest (streamTests) where

import Control.Comonad (Comonad (..))
import Data.Distributive (Distributive (..))
import Data.Functor.Rep (Representable (..))
import Data.Key (Lookup (..), mapWithKey)
import Lambda.Stream
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck
  ( Arbitrary (..),
    Fun,
    NonNegative (..),
    applyFun,
    testProperty,
  )
import Prelude hiding (lookup)

instance (Arbitrary a) => Arbitrary (Stream a) where
  arbitrary = tabulateS <$> arbitrary

-- | Helper to compare two streams for equality on their first 20 elements.
eqStreamPrefix :: (Eq a) => Stream a -> Stream a -> Bool
eqStreamPrefix s1 s2 = takeS 20 s1 == takeS 20 s2

streamTests :: TestTree
streamTests =
  testGroup
    "Stream (Coinductive Infinite Stream) Tests"
    [ testGroup
        "Core Operations & Classic Streams"
        [ testCase "natsS produces natural numbers 0..9" $
            takeS 10 natsS @?= [0 .. 9],
          testCase "fibsS produces first 10 Fibonacci numbers" $
            takeS 10 fibsS @?= [0, 1, 1, 2, 3, 5, 8, 13, 21, 34],
          testCase "alternatingBoolsS cycles True and False" $
            takeS 6 alternatingBoolsS @?= [True, False, True, False, True, False],
          testCase "interleaveS merges two streams alternately" $
            takeS 6 (interleaveS (repeatS (1 :: Int)) (repeatS 2)) @?= [1, 2, 1, 2, 1, 2],
          testCase "scanlS computes running prefix sums" $
            takeS 6 (scanlS (+) (0 :: Int) (repeatS 1)) @?= [0, 1, 2, 3, 4, 5],
          testProperty "headS and tailS reconstruct prefix" $
            \(s :: Stream Int) ->
              takeS 15 (Cons (headS s) (tailS s)) == takeS 15 s,
          testProperty "takeS n s ++ takeS m (dropS n s) == takeS (n + m) s" $
            \(NonNegative n) (NonNegative m) (s :: Stream Int) ->
              let n' = n `mod` 20
                  m' = m `mod` 20
               in takeS n' s ++ takeS m' (dropS n' s) == takeS (n' + m') s
        ],
      testGroup
        "Functor Laws"
        [ testProperty "Functor Identity: fmap id == id" $
            \(s :: Stream Int) ->
              eqStreamPrefix (fmap id s) s,
          testProperty "Functor Composition: fmap (f . g) == fmap f . fmap g" $
            \(f :: Fun Int Int) (g :: Fun Int Int) (s :: Stream Int) ->
              let f' = applyFun f
                  g' = applyFun g
               in eqStreamPrefix (fmap (f' . g') s) (fmap f' (fmap g' s))
        ],
      testGroup
        "Applicative Laws (Zipwise)"
        [ testProperty "Applicative Identity: pure id <*> v == v" $
            \(v :: Stream Int) ->
              eqStreamPrefix (pure id <*> v) v,
          testProperty "Applicative Homomorphism: pure f <*> pure x == pure (f x)" $
            \(f :: Fun Int Int) (x :: Int) ->
              let f' = applyFun f
               in eqStreamPrefix (pure f' <*> pure x) (pure (f' x)),
          testProperty "Applicative Interchange: u <*> pure y == pure ($ y) <*> u" $
            \(uFun :: Stream (Fun Int Int)) (y :: Int) ->
              let u = fmap applyFun uFun
               in eqStreamPrefix (u <*> pure y) (pure ($ y) <*> u)
        ],
      testGroup
        "Monad Laws (Diagonal)"
        [ testProperty "Monad Left Identity: (return a >>= k) == k a" $
            \(a :: Int) (kFun :: Fun Int (Stream Int)) ->
              let k = applyFun kFun
               in eqStreamPrefix (return a >>= k) (k a),
          testProperty "Monad Right Identity: (m >>= return) == m" $
            \(m :: Stream Int) ->
              eqStreamPrefix (m >>= return) m,
          testProperty "Monad Associativity: ((m >>= f) >>= g) == (m >>= (\\x -> f x >>= g))" $
            \(m :: Stream Int) (fFun :: Fun Int (Stream Int)) (gFun :: Fun Int (Stream Int)) ->
              let f = applyFun fFun
                  g = applyFun gFun
               in takeS 12 ((m >>= f) >>= g) == takeS 12 (m >>= (\x -> f x >>= g))
        ],
      testGroup
        "Comonad Laws"
        [ testProperty "Comonad Law 1: extract . duplicate == id" $
            \(w :: Stream Int) ->
              eqStreamPrefix (extract (duplicate w)) w,
          testProperty "Comonad Law 2: fmap extract . duplicate == id" $
            \(w :: Stream Int) ->
              eqStreamPrefix (fmap extract (duplicate w)) w,
          testProperty "Comonad Law 3: duplicate . duplicate == fmap duplicate . duplicate" $
            \(w :: Stream Int) ->
              takeS 8 (fmap (takeS 8 . fmap (takeS 8)) (duplicate (duplicate w)))
                == takeS 8 (fmap (takeS 8 . fmap (takeS 8)) (fmap duplicate (duplicate w)))
        ],
      testGroup
        "Representable, Distributive & Keyed Laws"
        [ testProperty "Representable Law 1: tabulate . index == id" $
            \(s :: Stream Int) ->
              eqStreamPrefix (tabulate (index s)) s,
          testProperty "Representable Law 2: index (tabulate f) i == f i" $
            \(fFun :: Fun Int Int) (NonNegative i) ->
              let f = applyFun fFun
                  idx = i `mod` 50
               in index (tabulate f :: Stream Int) idx == f idx,
          testProperty "Distributive distributes Maybe across Stream" $
            \(s :: Stream Int) ->
              takeS 10 (distribute (Just s)) == map Just (takeS 10 s),
          testProperty "Keyed lookup matches indexS" $
            \(s :: Stream Int) (NonNegative i) ->
              let idx = i `mod` 50
               in lookup idx s == Just (indexS s idx),
          testProperty "mapWithKey adds index to stream elements" $
            \(s :: Stream Int) ->
              takeS 10 (mapWithKey (+) s) == zipWith (+) [0 ..] (takeS 10 s)
        ],
      testGroup
        "Cofree Identity Isomorphism"
        [ testProperty "fromCofree . toCofree == id" $
            \(s :: Stream Int) ->
              eqStreamPrefix (fromCofree (toCofree s)) s
        ]
    ]
