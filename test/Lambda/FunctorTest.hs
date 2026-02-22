{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lambda.FunctorTest where

import Control.Monad.Reader
import Lambda.Functor (MyMaybe (..), MyReader (..), runMyReader)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes (functor)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tastyBatch :: TestBatch -> TestTree
tastyBatch (name, tests) = testProperties name tests

-- ==========================================
-- 1. Standard Maybe Tests
-- ==========================================
-- prop_maybeIdentity :: Maybe String -> Property
-- prop_maybeIdentity m = fmap id m === m

-- prop_maybeComposition :: Fun String Int -> Fun Int String -> Maybe Int -> Property
-- prop_maybeComposition (Fn f) (Fn g) m = fmap (f . g) m === (fmap f . fmap g) m

functorMaybeTests :: TestTree
functorMaybeTests =
  testGroup
    "Functor Maybe"
    [ testCase "Hardcoded Just 10" $ fmap id (Just (10 :: Int)) @?= Just (10 :: Int),
      testCase "Hardcoded Nothing" $ fmap id (Nothing :: Maybe Int) @?= Nothing,
      tastyBatch (functor (undefined :: Maybe (Int, String, Int)))
      -- , testProperty "Identity Law" prop_maybeIdentity
      -- , testProperty "Composition Law" prop_maybeComposition
    ]

-- ==========================================
-- 2. Custom MyMaybe Tests
-- ==========================================

instance (Arbitrary a) => Arbitrary (MyMaybe a) where
  arbitrary =
    frequency
      [ (1, pure MyNothing),
        (3, fmap MyJust arbitrary)
      ]

-- Tell checkers how to compare MyMaybe
instance (Eq a) => EqProp (MyMaybe a) where
  (=-=) = eq

-- Generate all functor tests automatically
functorMyMaybeTests :: TestTree
functorMyMaybeTests = tastyBatch $ functor (undefined :: MyMaybe (Int, String, Int))

-- ==========================================
-- 2. Custom MyMaybe Tests
-- ==========================================
-- instance Arbitrary a => Arbitrary (MyMaybe a) where
--     arbitrary = frequency
--         [ (1, pure MyNothing)
--         , (3, fmap MyJust arbitrary)
--         ]

-- prop_myFunctorIdentity :: MyMaybe String -> Property
-- prop_myFunctorIdentity m = fmap id m === m

-- prop_myFunctorComposition :: Fun Int String -> Fun String Int -> MyMaybe Int -> Property
-- prop_myFunctorComposition (Fn f) (Fn g) m = fmap (g . f) m === (fmap g . fmap f) m

-- functorMyMaybeTests :: TestTree
-- functorMyMaybeTests = testGroup "Functor MyMaybe"
--     [ testProperty "Identity Law" prop_myFunctorIdentity
--     , testProperty "Composition Law" prop_myFunctorComposition
--     ]

-- ==========================================
-- 3. Standard Reader (Function) Tests
-- ==========================================

-- A custom operator for "Extensional Equality" of Readers
eqReader :: (Eq a, Show a) => Reader r a -> Reader r a -> r -> Property
eqReader r1 r2 x = runReader r1 x === runReader r2 x

prop_readerIdentity :: Fun Int String -> Int -> Property
prop_readerIdentity (Fn rawR) x =
  let r = reader rawR
   in eqReader (fmap id r) r x

prop_readerComposition :: Fun String Int -> Fun Int String -> Fun Int Int -> Int -> Property
prop_readerComposition (Fn f) (Fn g) (Fn rawR) val =
  let r = reader rawR
      leftSide = fmap (f . g) r
      rightSide = (fmap f . fmap g) r
   in eqReader leftSide rightSide val

functorReaderTests :: TestTree
functorReaderTests =
  testGroup
    "Functor Reader"
    [ testProperty "Reader Identity Law" prop_readerIdentity,
      testProperty "Composition Law" prop_readerComposition
    ]

-- ==========================================
-- 4. Custom MyReader Tests
-- ==========================================

eqMyReader :: (Eq b, Show b) => MyReader a b -> MyReader a b -> a -> Property
eqMyReader m1 m2 x = runMyReader m1 x === runMyReader m2 x

prop_myReaderIdentity :: Fun Int String -> Int -> Property
prop_myReaderIdentity (Fn rawR) val =
  let r = MyReader rawR
   in eqMyReader (fmap id r) r val

prop_myReaderComposition :: Fun String Int -> Fun Int String -> Fun Int Int -> Int -> Property
prop_myReaderComposition (Fn f) (Fn g) (Fn rawR) val =
  let r = MyReader rawR
      leftSide = fmap (f . g) r
      rightSide = (fmap f . fmap g) r
   in eqMyReader leftSide rightSide val

functorMyReaderTests :: TestTree
functorMyReaderTests =
  testGroup
    "Functor MyReader"
    [ testProperty "Identity Law" prop_myReaderIdentity,
      testProperty "Composition Law" prop_myReaderComposition
    ]

-- ==========================================
-- Master Test Tree
-- ==========================================
functorTests :: TestTree
functorTests =
  testGroup
    "Functor Suite"
    [ functorMaybeTests,
      functorMyMaybeTests,
      functorReaderTests,
      functorMyReaderTests
    ]
