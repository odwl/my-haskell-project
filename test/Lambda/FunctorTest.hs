{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# HLINT ignore "Functor law" #-}

module Lambda.FunctorTest where

import Control.Monad ((>=>))
import Control.Monad.Reader (reader)
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.Maybe (isNothing)
import Lambda.Functor (MaybeList (..), MyMaybe (..), MyReader (..), calc, fishB, myDiv, process, process2, takeWhileM)
import Lambda.FunctorTestUtils (eqMyReader, eqReader)
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

functorMaybeTests :: TestTree
functorMaybeTests =
  testGroup
    "Functor Maybe"
    [ testCase "Hardcoded Just 10" $ fmap id (Just (10 :: Int)) @?= Just (10 :: Int),
      testCase "Hardcoded Nothing" $ fmap id (Nothing :: Maybe Int) @?= Nothing,
      tastyBatch (functor (undefined :: Maybe (Int, String, Int)))
    ]

-- ==========================================
-- 2. Custom MyMaybe Tests
-- ==========================================

-- Generate all functor tests automatically
functorMyMaybeTests :: TestTree
functorMyMaybeTests = tastyBatch $ functor (undefined :: MyMaybe (Int, String, Int))

-- ==========================================
-- 3. Standard Reader (Function) Tests
-- ==========================================

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

mathFunctionsTests :: TestTree
mathFunctionsTests =
  testGroup
    "Math Functions"
    [ testCase "myDiv 4 3" $ myDiv (4 :: Int) 3 @?= Just 1,
      testCase "myDiv 6 3" $ myDiv (6 :: Int) 3 @?= Just 2,
      testCase "myDiv 9 3 (The if-clause)" $ myDiv (9 :: Int) 3 @?= Nothing,
      testProperty "calc is commutative" prop_calcCommutative,
      testProperty "calc matches manual calculation" prop_calcCorrect,
      testCase "calc 4 2 (2 + 0)" $ calc (4 :: Int) 2 @?= Just 2,
      testCase "calc 1 1 (1 + 1)" $ calc (1 :: Int) 1 @?= Just 2,
      testCase "calc 6 2 (myDiv 6 2 is 3 -> Nothing)" $ calc (6 :: Int) 2 @?= Nothing,
      testCase "calc 4 0 (Nothing)" $ calc (4 :: Int) 0 @?= Nothing
    ]

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
      mathFunctionsTests,
      kleisliTests,
      takeWhileMTests,
      testProcess,
      testProcess2
    ]

testProcess :: TestTree
testProcess =
  testGroup
    "Process Tests"
    [ testCase "Valid positive numbers" $ do
        process 4.0 @?= Just 1.5
        process 16.0 @?= Just 1.25,
      testCase "Invalid numbers" $ do
        assertBool "0.0 should be Nothing" (isNothing (process 0.0))
        assertBool "0.0001 should be Nothing" (isNothing (process 0.0001))
        assertBool "-1.0 should be Nothing" (isNothing (process (-1.0)))
    ]

testProcess2 :: TestTree
testProcess2 =
  testGroup
    "Process2 Tests"
    [ testCase "Valid positive numbers" $ do
        process2 4.0 @?= Just 1.5
        process2 16.0 @?= Just 1.25,
      testCase "Invalid numbers" $ do
        assertBool "0.0 should be Nothing" (isNothing (process2 0.0))
        assertBool "0.0001 should be Nothing" (isNothing (process2 0.0001))
        assertBool "-1.0 should be Nothing" (isNothing (process2 (-1.0)))
    ]

-- ==========================================
-- 6. takeWhileM (Utility) Tests
-- ==========================================

-- Helper to include failing element purely
takeThrough :: (a -> Bool) -> [a] -> [a]
takeThrough _ [] = []
takeThrough p (x : xs)
  | p x = x : takeThrough p xs
  | otherwise = [x]

prop_takeWhileMIdentity :: (Int -> Bool) -> [Int] -> Property
prop_takeWhileMIdentity p xs =
  runIdentity (takeWhileM (Identity . p) xs) === takeThrough p xs

prop_calcCommutative :: Int -> Int -> Property
prop_calcCommutative l r = calc l r === calc r l

prop_calcCorrect :: Int -> Int -> Property
prop_calcCorrect l r =
  let expected = case (myDiv l r, myDiv r l) of
        (Just v1, Just v2) -> Just (v1 + v2)
        _ -> Nothing
   in calc l r === expected

takeWhileMTests :: TestTree
takeWhileMTests =
  testGroup
    "takeWhileM Utility"
    [ testProperty "Behaves like takeThrough (inclusive takeWhile)" prop_takeWhileMIdentity
    ]

kleisliTests :: TestTree
kleisliTests =
  testGroup
    "Kleisli Utilities"
    [ testProperty "fishB is equivalent to >=> (Maybe)" (prop_fishBEquivalentToKleisli @Maybe @Int @Int @Int),
      testProperty "fishB is equivalent to >=> (List)" (prop_fishBEquivalentToKleisli @[] @Int @Int @Int),
      testProperty "process is equivalent to process2" prop_processEquivalentToProcess2
    ]

prop_fishBEquivalentToKleisli :: forall m a b c. (Monad m, Eq (m c), Show (m c)) => Fun a (m b) -> Fun b (m c) -> a -> Property
prop_fishBEquivalentToKleisli (Fn f) (Fn g) x =
  fishB f g x === (f >=> g) x

prop_processEquivalentToProcess2 :: Float -> Property
prop_processEquivalentToProcess2 x =
  process x === process2 x
