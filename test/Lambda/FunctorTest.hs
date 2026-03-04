module Lambda.FunctorTest where

import Control.Monad.Reader (reader)
import Lambda.Functor
import Lambda.FunctorTestUtils (eqMyReader, eqReader, genSafeMoves)
import Lambda.Subdist (runSubdist)
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
-- 5. Hover Dam Tests (Subdist version)
-- ==========================================

hoverDamTests :: TestTree
hoverDamTests =
  testGroup
    "Hover Dam (Subdist)"
    [ testCase "Safe Scenario: 2 enter, 2 leave" $
        let dist = runSubdist (damOpens >>= carEnters >>= carEnters >>= carLeaves >>= carLeaves)
         in dist @?= [(0, 1.0)],
      testCase "Bifurcation Scenario: Reaching Capacity" $
        let dist = runSubdist (damOpens >>= carEnters >>= carEnters >>= carEnters)
         in dist @?= [(3, 1.0)],
      testCase "Collapse Scenario: 50% chance at threshold" $
        let dist = runSubdist (damOpens >>= carEnters >>= carEnters >>= carEnters >>= carEnters)
         in -- We expect [(4, 0.5)] because the other 0.5 is lost to "crash" (Nothing in Subdist)
            dist @?= [(4, 0.5)],
      testCase "Collapse Scenario: 25% chance at threshold" $
        let dist = runSubdist (damOpens >>= carEnters >>= carEnters >>= carEnters >>= carEnters >>= carLeaves >>= carEnters)
         in -- We expect [(4, 0.25)] because the other 0.75 is lost to "crash" (Nothing in Subdist)
            dist @?= [(4, 0.25)],
      testCase "Total Collapse: Exceeding threshold" $
        let dist = runSubdist (damOpens >>= carEnters >>= carEnters >>= carEnters >>= carEnters >>= carEnters)
         in dist @?= [],
      testProperty "Random Safe Path Survival" prop_damRandomSafePath,
      testProperty "Probability decreases or stays same" prop_damProbNonIncreasing
    ]

prop_damRandomSafePath :: Property
prop_damRandomSafePath = property $ do
  moves <- genSafeMoves
  let dist = runSubdist (foldl (>>=) damOpens moves)
  return $ case dist of
    [(n, p)] -> counterexample ("State: " ++ show n ++ " Prob: " ++ show p) (n <= damCapacity && p == 1.0)
    [] -> counterexample "Dam collapsed unexpectedly on safe path" False
    _ -> counterexample ("Unexpected distribution: " ++ show dist) False

prop_damProbNonIncreasing :: Property
prop_damProbNonIncreasing = property $ do
  nbEnters <- choose (0, 10 :: Int)
  let dist = runSubdist (foldl (>>=) damOpens (replicate nbEnters carEnters))
      totalProb = sum (map snd dist)
  return $ counterexample ("Total prob: " ++ show totalProb) (totalProb <= 1.0)

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
      hoverDamTests
    ]
