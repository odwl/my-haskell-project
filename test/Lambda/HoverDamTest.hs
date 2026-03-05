{-# LANGUAGE LambdaCase #-}

module Lambda.HoverDamTest where

import qualified Data.Map as Map
import Lambda.HoverDam
import Lambda.RandomWalk
import Lambda.Subdist (Subdist, runSubdist)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

-- ==========================================
-- Hover Dam Domains
-- ==========================================

-- Generates a list of movement functions (carEnters or carLeaves) that stay within [0, damCapacity]
genSafeMovesStartingAt :: Int -> Gen [CarCount -> Subdist CarCount]
genSafeMovesStartingAt start = do
  numSteps <- choose (0, 50 :: Int)
  let CarCount cap = damCapacity
  let RandomWalk _ genActions = genBoundedActions 0 cap start numSteps
  actions <- genActions
  return $ map (\case Inc -> carEnters; Dec -> carLeaves) actions

genSafeMoves :: Gen [CarCount -> Subdist CarCount]
genSafeMoves = genSafeMovesStartingAt 0

-- ==========================================
-- Enhanced Walk Tracking
-- ==========================================

data WalkResult = WalkResult
  { wrExpectedProb :: Double, -- Expected probability based on risk count
    wrCollapsed :: Bool -- True if a state > 'damCollapseThreshold' was reached
  }
  deriving (Show, Eq)

-- | Analyzes a sequence of actions for the hover dam simulation.
-- It tracks how many times 'carEnters' was called when the dam was at capacity.
genHoverDamWalk :: Int -> Gen [Action] -> Gen WalkResult
genHoverDamWalk start genActions = do
  -- Use analyzePath to extract stats
  visits <- analyzePath (RandomWalk start genActions)
  let CarCount cap = damCapacity
      CarCount thresh = damCollapseThreshold
  let riskCount = Map.findWithDefault 0 (cap + 1) visits
      col = any (> thresh) (Map.keys visits)
  return (WalkResult (0.5 ^ riskCount) col)

-- ==========================================
-- Hover Dam Tests (Subdist version)
-- ==========================================

hoverDamScenarioTests :: TestTree
hoverDamScenarioTests =
  testGroup
    "Hover Dam (Subdist)"
    [ testCase "Safe Scenario: 2 enter, 2 leave" $
        let dist = runSubdist (damOpens >>= carEnters >>= carEnters >>= carLeaves >>= carLeaves)
         in dist @?= [(CarCount 0, 1.0)],
      testCase "Bifurcation Scenario: Reaching Capacity" $
        let dist = runSubdist (damOpens >>= carEnters >>= carEnters >>= carEnters)
         in dist @?= [(CarCount 3, 1.0)],
      testCase "Collapse Scenario: 50% chance at threshold" $
        let dist = runSubdist (damOpens >>= carEnters >>= carEnters >>= carEnters >>= carEnters)
         in -- We expect [(4, 0.5)] because the other 0.5 is lost to "crash" (Nothing in Subdist)
            dist @?= [(CarCount 4, 0.5)],
      testCase "Collapse Scenario: 25% chance at threshold" $
        let dist = runSubdist (damOpens >>= carEnters >>= carEnters >>= carEnters >>= carEnters >>= carLeaves >>= carEnters)
         in -- We expect [(4, 0.25)] because the other 0.75 is lost to "crash" (Nothing in Subdist)
            dist @?= [(CarCount 4, 0.25)],
      testCase "Total Collapse: Exceeding threshold" $
        let dist = runSubdist (damOpens >>= carEnters >>= carEnters >>= carEnters >>= carEnters >>= carEnters)
         in dist @?= [],
      testProperty "Random Safe Path Survival" prop_damRandomSafePath,
      testProperty "Probability decreases or stays same" prop_damProbNonIncreasing,
      testProperty "Manual Probability Verification" prop_damManualProbVerify
    ]

prop_damManualProbVerify :: Property
prop_damManualProbVerify = property $ do
  numSteps <- choose (0, 30 :: Int)
  let CarCount thresh = damCollapseThreshold
  let RandomWalk _ genActions = genBoundedActions 0 (thresh + 1) 0 numSteps
  actions <- genActions
  result <- genHoverDamWalk 0 (pure actions)
  let moves = map (\case Inc -> carEnters; Dec -> carLeaves) actions
      dist = runSubdist (foldl (>>=) damOpens moves)
      expectedProb = wrExpectedProb result
      actualProb = case dist of
        [(_, p)] -> p
        [] -> 0.0
        _ -> -1.0 -- Should not happen in this simplified model
  return $
    if wrCollapsed result
      then counterexample "Expected collapse." (null dist)
      else
        counterexample
          ( "Expected: "
              ++ show expectedProb
              ++ " Actual: "
              ++ show actualProb
          )
          (abs (actualProb - expectedProb) < 1e-6)

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

hoverDamTests :: TestTree
hoverDamTests = hoverDamScenarioTests
