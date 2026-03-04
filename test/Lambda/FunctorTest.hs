{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lambda.FunctorTest where

import Control.Monad (foldM)
import Control.Monad.Reader
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Lambda.Functor (MaybeList (..), MyMaybe (..), MyReader (..), carEnters, carLeaves, damCapacity, damOpens, runMyReader)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes (applicative, functor, monad)
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
-- 5. MaybeList Tests
-- ==========================================

instance (Arbitrary a) => Arbitrary (MaybeList a) where
  arbitrary = fmap MaybeList (scale (`div` 3) arbitrary)

instance (Eq a) => EqProp (MaybeList a) where
  (=-=) :: (Eq a) => MaybeList a -> MaybeList a -> Property
  (=-=) = eq

functorMaybeListTests :: TestTree
functorMaybeListTests =
  testGroup
    "Functor MaybeList"
    [tastyBatch $ functor (undefined :: MaybeList (Int, String, Int))]

applicativeMaybeListTests :: TestTree
applicativeMaybeListTests =
  testGroup
    "Applicative MaybeList"
    [tastyBatch $ applicative (undefined :: MaybeList (Int, String, Int))]

monadMaybeListTests :: TestTree
monadMaybeListTests =
  testGroup
    "Monad MaybeList"
    [tastyBatch $ monad (undefined :: MaybeList (Int, String, Int))]

-- ==========================================
-- 6. Hover Dam Tests
-- ==========================================

hoverDamTests :: TestTree
hoverDamTests =
  testGroup
    "Hover Dam"
    [ testCase "Safe Scenario: 2 enter, 2 leave" $
        (damOpens >>= carEnters >>= carEnters >>= carLeaves >>= carLeaves) @?= Just 0,
      testCase "Collapse Scenario: Exceeding 3 cars" $
        (damOpens >>= carEnters >>= carEnters >>= carEnters >>= carEnters >>= carLeaves >>= carLeaves) @?= Nothing,
      testProperty "Random Safe Path Survival" prop_damRandomPathSurvival
    ]

-- Generates a single move that is guaranteed to be safe from the current state.
genSafeNextMove :: Int -> Gen (Int -> Maybe Int)
genSafeNextMove 0 = pure carEnters
genSafeNextMove n | n < damCapacity = elements [carEnters, carLeaves]
genSafeNextMove _ = pure carLeaves

-- Generates a final state by applying `genSafeNextMove` `steps` times from `startState`.
genSafeStateAfterSteps :: Int -> Int -> Gen (Maybe Int)
genSafeStateAfterSteps steps startState =
  runMaybeT $
    foldM
      ( \curr _ -> do
          move <- lift $ genSafeNextMove curr
          MaybeT $ pure (move curr)
      )
      startState
      [1 .. steps]

prop_damRandomPathSurvival :: Property
prop_damRandomPathSurvival = property $ do
  state <- choose (0, damCapacity)
  steps <- choose (0, 50)
  finalStateMaybe <- genSafeStateAfterSteps steps state
  let safePathSurvived = finalStateMaybe /= Nothing
  let collapsedState = do
        nbCar <- finalStateMaybe
        let collapsingMoves = replicate (damCapacity + 1 - nbCar) carEnters
        foldl (>>=) finalStateMaybe collapsingMoves

  -- The safe path must survive, AND adding just enough cars must ALWAYS cause a collapse
  return $ safePathSurvived && collapsedState == Nothing

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
      functorMyReaderTests,
      functorMaybeListTests,
      applicativeMaybeListTests,
      monadMaybeListTests,
      hoverDamTests
    ]
