{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lambda.SubdistTest where

import Control.Monad (join, (>=>))
import Data.List (sort)
import Lambda.Subdist (Subdist, makeSubdist, runSubdist, simplify)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes (applicative, functor, monad)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tastyBatch :: TestBatch -> TestTree
tastyBatch (name, tests) = testProperties name tests

-- ==========================================
-- Subdist Generators and EqProp
-- ==========================================

instance (Arbitrary a) => Arbitrary (Subdist a) where
  arbitrary = do
    -- Generate small lists for performance
    n <- choose (1, 5)
    xs <- vectorOf n arbitrary
    -- Generate positive weights
    weights <- vectorOf n (getPositive <$> arbitrary)
    let totalWeight = sum weights
        -- Normalize weights so the sum is <= 1.0 (sub-probability)
        scaleFactor = if totalWeight > 1.0 then totalWeight + 0.1 else 1.0
        normalized = map (/ scaleFactor) weights
    case makeSubdist (zip xs normalized) of
      Just d -> return d
      Nothing -> error "Arbitrary Subdist generated invalid probabilities!"

-- Compare distributions after consolidating identical elements
instance (Eq a, Ord a, EqProp a, Show a) => EqProp (Subdist a) where
  d1 =-= d2 =
    let s1 = sort . runSubdist $ simplify d1
        s2 = sort . runSubdist $ simplify d2

        approxEq p1 p2 = abs (p1 - p2) < 1e-6

        compareDists [] [] = property True
        compareDists ((v1, p1) : r1) ((v2, p2) : r2) =
          (v1 =-= v2)
            .&&. counterexample ("Probabilities differ: " ++ show p1 ++ " != " ++ show p2) (approxEq p1 p2)
            .&&. compareDists r1 r2
        compareDists _ _ = property False
     in compareDists s1 s2

-- ==========================================
-- Law Verification
-- ==========================================

functorSubdistTests :: TestTree
functorSubdistTests = tastyBatch $ functor (undefined :: Subdist (Int, String, Int))

applicativeSubdistTests :: TestTree
applicativeSubdistTests = tastyBatch $ applicative (undefined :: Subdist (Int, String, Int))

monadSubdistTests :: TestTree
monadSubdistTests = tastyBatch $ monad (undefined :: Subdist (Int, String, Int))

-- ==========================================
-- Example Usage
-- ==========================================

-- | A biased coin flip
coinFlip :: Subdist String
coinFlip = case makeSubdist [("Heads", 0.6), ("Tails", 0.2)] of
  Just d -> d
  Nothing -> error "Invalid coinFlip distribution"

-- | Two independent flips
twoFlips :: Subdist (String, String)
twoFlips = liftA2 (,) coinFlip coinFlip

exampleTests :: TestTree
exampleTests =
  testGroup
    "Subdist Examples"
    [ testCase "Two Biased Flips" $ do
        let expected =
              [ (("Heads", "Heads"), 0.36),
                (("Heads", "Tails"), 0.12),
                (("Tails", "Heads"), 0.12),
                (("Tails", "Tails"), 0.04)
              ]
            actual = runSubdist twoFlips
        length actual @?= length expected
        mapM_
          ( \((v1, p1), (v2, p2)) -> do
              v1 @?= v2
              assertBool ("Probabilities differ: " ++ show p1 ++ " != " ++ show p2) (abs (p1 - p2) < 1e-6)
          )
          (zip actual expected)
    ]

subdistTests :: TestTree
subdistTests =
  testGroup
    "Subdist Suite"
    [ functorSubdistTests,
      applicativeSubdistTests,
      monadSubdistTests,
      exampleTests,
      kleisliTests
    ]

-- ==========================================
-- Example Logic and Kleisli tests
-- ==========================================

myDist :: Subdist Bool
myDist = case makeSubdist [(True, 0.8), (False, 0.2)] of
  Just d -> d
  Nothing -> error "Invalid test distribution"

exp1 :: Subdist Char
exp1 = case makeSubdist [('H', 0.6), ('L', 0.4)] of
  Just d -> d
  Nothing -> error "Invalid test distribution"

exp2 :: Subdist Char
exp2 = pure 'L'

g :: Bool -> Subdist Char
g True = exp1
g False = exp2

f :: () -> Subdist Bool
f () = myDist

expectedOutcome :: Subdist Char
expectedOutcome = case makeSubdist [('H', 0.48), ('L', 0.52)] of
  Just d -> d
  Nothing -> error "Invalid test distribution"

kleisliTests :: TestTree
kleisliTests =
  testGroup
    "Kleisli Tests"
    [ testCase "g <$> myDist matches manual expected" $ do
        (g <$> myDist) @?= case makeSubdist [(exp1, 0.8), (exp2, 0.2)] of
          Just d -> d
          Nothing -> error "Invalid test distribution",
      testCase "join (g <$> myDist) == expectedOutcome" $ do
        join (g <$> myDist) @?= expectedOutcome,
      testCase "(f >=> g) () == expectedOutcome" $ do
        (f >=> g) () @?= expectedOutcome,
      testProperty "join . fmap g . f  vs  f >=> g  equivalence property" $
        let leftSide = join . fmap g . f
            rightSide = f >=> g
         in leftSide =-= rightSide
    ]
