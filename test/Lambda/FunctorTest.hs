module Lambda.FunctorTest where

import Control.Monad.Reader (reader)
import Lambda.Functor (MaybeList (..), MyMaybe (..), MyReader (..), carEnters, carLeaves, damCapacity, damOpens)
import Lambda.FunctorTestUtils (eqMyReader, eqReader, genSafeMoves, genSafeMovesStartingAt)
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

-- ==========================================
-- 6. Hover Dam Tests
-- ==========================================

hoverDamTests :: TestTree
hoverDamTests =
  testGroup
    "Hover Dam"
    [ testCase "Safe Scenario: 2 enter, 2 leave" $
        (damOpens >>= carEnters >>= carEnters >>= carLeaves >>= carLeaves) @?= MaybeList [Just 0],
      testCase "Collapse Scenario: Exceeding 3 cars" $
        (damOpens >>= carEnters >>= carEnters >>= carEnters >>= carEnters >>= carLeaves >>= carLeaves) @?= MaybeList [Just 2, Nothing],
      testProperty "Random Safe Path Survival" prop_damRandomSafePath,
      testProperty "Random Danger Path Recovery" prop_damRandomDangerPath,
      testProperty "Bifurcation and Collapse" prop_damBifurcationAndCollapse,
      testProperty "MaybeList carEnters Consistency" prop_maybeListEnterConsistency
    ]

prop_maybeListEnterConsistency :: MaybeList Int -> Property
prop_maybeListEnterConsistency ml =
  (ml >>= carEnters) === manualEnter ml
  where
    manualEnter (MaybeList ms) = MaybeList $ concatMap applyEnter ms
    applyEnter Nothing = [Nothing]
    applyEnter (Just n) = getMaybeList (carEnters n)

prop_damRandomSafePath :: Property
prop_damRandomSafePath = property $ do
  MaybeList lastSafeState <- foldl (>>=) damOpens <$> genSafeMoves
  return $ case lastSafeState of [Just n] -> n <= damCapacity; _ -> False

prop_damRandomDangerPath :: Property
prop_damRandomDangerPath = property $ do
  let state = foldl (>>=) damOpens (replicate 4 carEnters ++ [carLeaves])
  -- 1. Check initial recovery to [Just 3, Nothing]
  if state /= MaybeList [Just 3, Nothing]
    then return $ counterexample ("Initial recovery failed: " ++ show state) False
    else do
      -- 2. Continue with safe random walk from 3
      moves <- genSafeMovesStartingAt 3
      let MaybeList finalState = foldl (>>=) state moves
      return $ case finalState of
        [Just n, Nothing] -> counterexample ("Final state out of bounds: " ++ show n) (n <= damCapacity)
        other -> counterexample ("Unexpected final state (expected exactly one healthy path): " ++ show other) False

prop_damBifurcationAndCollapse :: Property
prop_damBifurcationAndCollapse = property $ do
  -- 1. Pick a random valid car count [0..damCapacity]
  nbCars <- choose (0, damCapacity)
  -- 2. Push that state to damCapacity, then enter once: must bifurcate
  let movesToDanger = replicate (damCapacity - nbCars + 1) carEnters
      atDangerCapacity = foldl (>>=) (pure nbCars) movesToDanger
      bifurcates = getMaybeList atDangerCapacity == [Just (damCapacity + 1), Nothing]
      -- 3. One more enter: all paths must collapse to Nothing
      allCollapsed = getMaybeList (atDangerCapacity >>= carEnters) == [Nothing, Nothing]
  return $
    counterexample ("nbCars=" ++ show nbCars) $
      counterexample ("atDangerCapacity=" ++ show (getMaybeList atDangerCapacity)) $
        counterexample ("bifurcates=" ++ show bifurcates ++ " allCollapsed=" ++ show allCollapsed) $
          bifurcates && allCollapsed

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
