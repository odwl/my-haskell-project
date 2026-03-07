{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Functor law" -}
{- HLINT ignore "Redundant fmap" -}

module Lambda.FunctorTest where

import Control.Monad ((>=>))
import Control.Monad.Reader (reader)
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.Maybe (fromMaybe, isNothing)
import Lambda.Functor (DamState (..), MaybeList (..), MyMaybe (..), MyReader (..), calc, capacity, checkOverflow, empty, fishB, myDiv, oneDay, sqrtInvAddOne, sqrtInvAddOneKleisli, takeWhileM)
import Lambda.FunctorTestUtils (eqMyReader, eqReader)
import Lambda.Subdist (Subdist, certainly, makeSubdist)
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
      testSqrtInvAddOne,
      testSqrtInvAddOneKleisli,
      waterSimulationTests
    ]

-- ==========================================
-- 7. Water Simulation Tests
-- ==========================================

safeSubdist :: [(DamState, Double)] -> Subdist DamState
safeSubdist = fromMaybe (error "Invalid probability distribution in test expected value") . makeSubdist

-- | Helper function to reduce boilerplate in test definitions.
--
-- Arguments:
-- 1. name     - Name of the testcase.
-- 2. sim      - The simulation function (or monadic chain) to evaluate.
-- 3. start    - The initial state to pass into the simulation.
-- 4. expected - The expected Subdist outcome to match against.
testSim :: String -> (a -> Subdist DamState) -> a -> Subdist DamState -> TestTree
testSim name sim start expected = testCase name $ sim start @?= expected

waterSimulationCases :: [(String, () -> Subdist DamState, Subdist DamState)]
waterSimulationCases =
  [ ( "empty returns 0 and 1",
      empty,
      certainly (OK 0)
    ),
    ( "checkOverflow handles values above capacity",
      const (checkOverflow (OK (capacity + 5))),
      certainly Overflowed
    ),
    ( "oneDay 0 matches expectedWater",
      empty >=> oneDay,
      safeSubdist [(OK 5, 0.8), (OK 0, 0.2)]
    ),
    ( "oneDay check overflow",
      empty >=> oneDay >=> checkOverflow,
      safeSubdist [(OK 5, 0.8), (OK 0, 0.2)]
    ),
    ( "twoDays matches consolidated expected",
      empty >=> oneDay >=> oneDay,
      safeSubdist [(OK 10, 0.64), (OK 5, 0.16), (OK 0, 0.20)]
    ),
    ( "four days check overflow consolidated expected",
      empty >=> oneDay >=> oneDay >=> oneDay >=> oneDay >=> checkOverflow,
      safeSubdist [(Overflowed, 0.4096), (OK 15, 0.1024), (OK 10, 0.3328), (OK 5, 0.0832), (OK 0, 0.072)]
    )
  ]

waterSimulationTests :: TestTree
waterSimulationTests =
  testGroup
    "Water Simulation"
    $ map (\(n, s, e) -> testSim n s () e) waterSimulationCases

testSqrtInvAddOne :: TestTree
testSqrtInvAddOne =
  testGroup
    "SqrtInvAddOne Tests"
    [ testCase "Valid positive numbers" $ do
        sqrtInvAddOne 4.0 @?= Just 1.5
        sqrtInvAddOne 16.0 @?= Just 1.25,
      testCase "Invalid numbers" $ do
        assertBool "0.0 should be Nothing" (isNothing (sqrtInvAddOne 0.0))
        assertBool "0.0001 should be Nothing" (isNothing (sqrtInvAddOne 0.0001))
        assertBool "-1.0 should be Nothing" (isNothing (sqrtInvAddOne (-1.0)))
    ]

testSqrtInvAddOneKleisli :: TestTree
testSqrtInvAddOneKleisli =
  testGroup
    "SqrtInvAddOneKleisli Tests"
    [ testCase "Valid positive numbers" $ do
        sqrtInvAddOneKleisli 4.0 @?= Just 1.5
        sqrtInvAddOneKleisli 16.0 @?= Just 1.25,
      testCase "Invalid numbers" $ do
        assertBool "0.0 should be Nothing" (isNothing (sqrtInvAddOneKleisli 0.0))
        assertBool "0.0001 should be Nothing" (isNothing (sqrtInvAddOneKleisli 0.0001))
        assertBool "-1.0 should be Nothing" (isNothing (sqrtInvAddOneKleisli (-1.0)))
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
      testProperty "sqrtInvAddOne is equivalent to sqrtInvAddOneKleisli" prop_sqrtInvAddOneEquivalentToKleisli
    ]

prop_fishBEquivalentToKleisli :: forall m a b c. (Monad m, Eq (m c), Show (m c)) => Fun a (m b) -> Fun b (m c) -> a -> Property
prop_fishBEquivalentToKleisli (Fn f) (Fn g) x =
  fishB f g x === (f >=> g) x

prop_sqrtInvAddOneEquivalentToKleisli :: Float -> Property
prop_sqrtInvAddOneEquivalentToKleisli x =
  sqrtInvAddOne x === sqrtInvAddOneKleisli x
