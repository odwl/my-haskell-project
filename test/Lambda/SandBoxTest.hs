{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lambda.SandBoxTest (sandBoxSuite) where

import Control.Arrow (Arrow (..), ArrowChoice (..), ArrowZero (..), (>>>))
import Control.Category (id, (.))
import Control.Comonad (Comonad (..))
import Control.Natural ((#))
import Data.Coerce (coerce)
import Data.Distributive (distribute)
import Data.Functor.Adjunction (Adjunction (..))
import Data.Functor.Alt (Alt (..))
import Data.Functor.Compose (Compose (..))
import Data.Functor.Extend (Extend (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Rep (Representable (..))
import Data.Functor.Yoneda (liftYoneda, runYoneda)
import Data.Key (Lookup (..), mapWithKey)
import Data.Profunctor (Profunctor (..))
import Fib.Algo (fib, fibFold, fibLog, fibLogCPS, fibLogFold)
import Lambda.SandBox (DeltaF (..), DoubleIdentity (..), MyIdentity (..), MyProxy (..), MyReader (..), UnitF (..), WriterKleisli (..), Zero (..), doubleToSingle, eitherBoolToNat, eitherBoolToNat', halve, maybeBoolToNat, maybeBoolToNat', sTail, sTail', sTail'', third, third')
import Prelude hiding (id, (.))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (Arbitrary (..), CoArbitrary, Fun (..), applyFun, discard, testProperty, withMaxSuccess, (==>))

instance Arbitrary (UnitF a) where
  arbitrary = pure (UnitF ())

instance Arbitrary a => Arbitrary (DeltaF a) where
  arbitrary = DeltaF <$> arbitrary

instance Arbitrary (Zero a) where
  arbitrary = discard -- Discards tests because Void is uninhabited



instance Arbitrary (MyProxy a) where
  arbitrary = pure MyProxy

instance Arbitrary a => Arbitrary (MyIdentity a) where
  arbitrary = MyIdentity <$> arbitrary

instance Arbitrary a => Arbitrary (DoubleIdentity a) where
  arbitrary = do
    (x :: a) <- arbitrary
    pure (coerce (MyIdentity x) :: DoubleIdentity a)

instance (CoArbitrary r, Arbitrary a) => Arbitrary (MyReader r a) where
  arbitrary = MyReader <$> arbitrary

instance Show (MyReader r a) where
  show _ = "MyReader <function>"

eqReader :: Eq a => r -> MyReader r a -> MyReader r a -> Bool
eqReader r (MyReader f1) (MyReader f2) = f1 r == f2 r



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
        "Yoneda Lemma & Isomorphisms"
        [ testProperty "Yoneda Lemma Isomorphism: runYoneda (liftYoneda m) f == maybeBoolToNat m # f" $
            \(m :: Maybe Bool) (fun :: Fun Bool Int) ->
              let f' = applyFun fun
               in runYoneda (liftYoneda m) f' == (maybeBoolToNat m # f'),
          testProperty "maybeBoolToNat == maybeBoolToNat' Equivalence: maybeBoolToNat m # f == maybeBoolToNat' m # f" $
            \(m :: Maybe Bool) (fun :: Fun Bool Int) ->
              let f' = applyFun fun
               in (maybeBoolToNat m # f') == (maybeBoolToNat' m # f'),
          testProperty "Either Yoneda Lemma Isomorphism: runYoneda (liftYoneda m) f == eitherBoolToNat m # f" $
            \(m :: Either () Bool) (fun :: Fun Bool Int) ->
              let f' = applyFun fun
               in runYoneda (liftYoneda m) f' == (eitherBoolToNat m # f'),
          testProperty "eitherBoolToNat == eitherBoolToNat' Equivalence: eitherBoolToNat m # f == eitherBoolToNat' m # f" $
            \(m :: Either () Bool) (fun :: Fun Bool Int) ->
              let f' = applyFun fun
               in (eitherBoolToNat m # f') == (eitherBoolToNat' m # f'),
          testProperty "DoubleIdentity <-> MyIdentity Isomorphism (L -> R)" $
            \(mi :: MyIdentity Int) -> doubleToSingle (coerce mi) == mi,
          testProperty "DoubleIdentity <-> MyIdentity Isomorphism (R -> L)" $
            \(di :: DoubleIdentity Int) -> coerce (doubleToSingle di) == di
        ],
      testGroup
        "UnitF Functor Laws"
        [ testProperty "Identity Law: fmap id == id" $
            \(u :: UnitF Int) ->
              fmap id u == u,
          testProperty "Composition Law: fmap (f . g) == fmap f . fmap g" $
            \(u :: UnitF Int) ->
              fmap (f . g) u == (fmap f . fmap g) u
        ],
      testGroup
        "DeltaF Functor Laws"
        [ testProperty "Identity Law: fmap id == id" $
            \(d :: DeltaF Int) ->
              fmap id d == d,
          testProperty "Composition Law: fmap (f . g) == fmap f . fmap g" $
            \(d :: DeltaF Int) ->
              fmap (f . g) d == (fmap f . fmap g) d
        ],
      testGroup
        "Zero Functor Laws"
        [ testProperty "Identity Law: fmap id == id" $
            withMaxSuccess 0 $ \(z :: Zero Int) ->
              fmap id z == z,
          testProperty "Composition Law: fmap (f . g) == fmap f . fmap g" $
            withMaxSuccess 0 $ \(z :: Zero Int) ->
              fmap (f . g) z == (fmap f . fmap g) z,
          testProperty "Alt Associativity: (a <!> b) <!> c == a <!> (b <!> c)" $
            withMaxSuccess 0 $ \(a :: Zero Int, b :: Zero Int, c :: Zero Int) ->
              ((a <!> b) <!> c) == (a <!> (b <!> c)),
          testProperty "Comonad Extract Law" $
            withMaxSuccess 0 $ \(z :: Zero Int) ->
              extract z == extract z
        ],
      testGroup
        "MyProxy Laws"
        [ testProperty "Functor Identity" $
            \(p :: MyProxy Int) -> fmap id p == p,
          testProperty "Functor Composition" $
            \(p :: MyProxy Int) -> fmap (f . g) p == (fmap f . fmap g) p,
          testProperty "Applicative Identity" $
            \(p :: MyProxy Int) -> (pure id <*> p) == p,
          testProperty "Applicative Homomorphism" $
            \(x :: Int) -> (pure f <*> pure x :: MyProxy Int) == pure (f x),
          testProperty "Applicative Interchange" $
            \(u :: MyProxy (Int -> Int), y :: Int) -> (u <*> pure y) == (pure ($ y) <*> u),
          testProperty "Keyed Identity" $
            \(p :: MyProxy Int) -> mapWithKey (\_ x -> x) p == p,
          testProperty "Distributive Law" $
            \(p :: MyProxy Int) -> distribute (Identity p) == fmap Identity p,
          testProperty "Alt Associativity" $
            \(a :: MyProxy Int, b :: MyProxy Int, c :: MyProxy Int) ->
              ((a <!> b) <!> c) == (a <!> (b <!> c)),
          testProperty "Extend Co-associativity" $
            \(p :: MyProxy Int) -> duplicated (duplicated p) == fmap duplicated (duplicated p),
          testProperty "Representable Tabulate-Index" $
            \(p :: MyProxy Int) -> tabulate (index p) == p,
          testProperty "Zero Functor Identity" $
            withMaxSuccess 0 $ \(z :: Zero Int) -> fmap id z == z,
          testProperty "Adjunction Triangular Law 1: counit . fmap unit == id" $
            withMaxSuccess 0 $ \(z :: Zero Int) -> counit (fmap unit z :: Zero (MyProxy (Zero Int))) == z,
          testProperty "Adjunction Triangular Law 2: fmap counit . unit == id" $
            withMaxSuccess 0 $ \(p :: MyProxy Int) -> fmap counit (unit p :: MyProxy (Zero (MyProxy Int))) == p
        ],
      testGroup
        "MyIdentity Laws"
        [ testProperty "Functor Identity" $
            \(mi :: MyIdentity Int) -> fmap id mi == mi,
          testProperty "Functor Composition" $
            \(mi :: MyIdentity Int) -> fmap (f . g) mi == (fmap f . fmap g) mi,
          testProperty "Applicative Identity" $
            \(mi :: MyIdentity Int) -> (pure id <*> mi) == mi,
          testProperty "Applicative Homomorphism" $
            \(x :: Int) -> (pure f <*> pure x :: MyIdentity Int) == pure (f x),
          testProperty "Applicative Interchange" $
            \(u :: MyIdentity (Fun Int Int), y :: Int) -> 
              let u' = fmap applyFun u 
               in (u' <*> pure y) == (pure ($ y) <*> u'),
          testProperty "Keyed Identity" $
            \(mi :: MyIdentity Int) -> mapWithKey (\_ x -> x) mi == mi,
          testProperty "Keyed map matches fmap" $
            \(mi :: MyIdentity Int) -> mapWithKey (\_ x -> f x) mi == fmap f mi,
          testProperty "Distributive Law" $
            \(mi :: MyIdentity Int) -> distribute (Identity mi) == fmap Identity mi,
          testProperty "Alt Associativity" $
            \(a :: MyIdentity Int, b :: MyIdentity Int, c :: MyIdentity Int) ->
              ((a <!> b) <!> c) == (a <!> (b <!> c)),
          testProperty "Extend Co-associativity" $
            \(mi :: MyIdentity Int) -> duplicated (duplicated mi) == fmap duplicated (duplicated mi),
          testProperty "Comonad Left/Right Identity" $
            \(mi :: MyIdentity Int) -> extract (duplicated mi) == mi && fmap extract (duplicated mi) == mi,
          testProperty "Representable Tabulate-Index" $
            \(mi :: MyIdentity Int) -> tabulate (index mi) == mi,
          testProperty "Lookup Identity" $
            \(mi :: MyIdentity Int) -> Data.Key.lookup () mi == Just (extract mi)
        ],
      testGroup
        "MyReader Laws"
        [ testProperty "Functor Identity" $
            \(r :: Int, mr :: MyReader Int Int) -> eqReader r (fmap id mr) mr,
          testProperty "Functor Composition" $
            \(r :: Int, mr :: MyReader Int Int) -> eqReader r (fmap (f . g) mr) ((fmap f . fmap g) mr),
          testProperty "Applicative Identity" $
            \(r :: Int, mr :: MyReader Int Int) -> eqReader r (pure id <*> mr) mr,
          testProperty "Applicative Homomorphism" $
            \(r :: Int, x :: Int) -> eqReader r (pure f <*> pure x :: MyReader Int Int) (pure (f x)),
          testProperty "Applicative Interchange" $
            \(r :: Int, u :: MyReader Int (Fun Int Int), y :: Int) -> 
              let u' = fmap applyFun u 
               in eqReader r (u' <*> pure y) (pure ($ y) <*> u'),
          testProperty "Keyed Identity" $
            \(r :: Int, mr :: MyReader Int Int) -> eqReader r (mapWithKey (\_ x -> x) mr) mr,
          testProperty "Keyed map matches fmap" $
            \(r :: Int, mr :: MyReader Int Int) -> eqReader r (mapWithKey (\_ x -> f x) mr) (fmap f mr),
          testProperty "Alt Associativity" $
            \(r :: Int, a :: MyReader Int Int, b :: MyReader Int Int, c :: MyReader Int Int) ->
              eqReader r ((a <!> b) <!> c) (a <!> (b <!> c)),
          testProperty "Extend Co-associativity" $
            \(r :: Int, mr :: MyReader Int Int) ->
              eqReader r (runMyReader (runMyReader (duplicated (duplicated mr)) r) r) (runMyReader (runMyReader (fmap duplicated (duplicated mr)) r) r),
          testProperty "Distributive Law" $
            \(r :: Int, mr :: MyReader Int Int) ->
              eqReader r (distribute (Identity mr)) (fmap Identity mr),
          testProperty "Representable Tabulate-Index" $
            \(r :: Int, mr :: MyReader Int Int) ->
              eqReader r (tabulate (index mr)) mr
        ],
      testGroup
        "FibFold Tests"
        [ testCase "fib 100 == 354224848179261915075" $
            fib 100 @?= 354224848179261915075,
          testCase "fibFold 100 == 354224848179261915075" $
            fibFold 100 @?= 354224848179261915075,
          testCase "fibLog 100 == 354224848179261915075" $
            fibLog 100 @?= 354224848179261915075,
          testCase "fibLogCPS 100 == 354224848179261915075" $
            fibLogCPS 100 @?= 354224848179261915075,
          testCase "fibLogFold 100 == 354224848179261915075" $
            fibLogFold 100 @?= 354224848179261915075
        ]
    ]




