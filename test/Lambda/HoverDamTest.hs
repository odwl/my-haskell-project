{-# LANGUAGE LambdaCase #-}

module Lambda.HoverDamTest where

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

-- | Converts a RandomWalk Action to a HoverDam movement function for a given strategy.
actionToMove :: DamStrategy -> Action -> (CarCount -> Subdist CarCount)
actionToMove strategy = \case
  Inc -> carEnters strategy
  Dec -> carLeaves

genRandomActions :: Gen [Action]
genRandomActions = do
  numSteps <- choose (0, 50 :: Int)
  take numSteps <$> genInfiniteActions

-- | Calculates the theoretical expected result of a sequence of actions.
-- This is the Single Source of Truth for test expectations, deriving its
-- logic directly from the given 'DamStrategy'.
calculateExpectedResult :: DamStrategy -> [Action] -> Int -> [(CarCount, Double)]
calculateExpectedResult strategy actions start =
  let (f, p, col) = foldl step (start, 1.0, False) actions
   in if col then [] else [(CarCount f, p)]
  where
    step (s, p, col) Inc =
      let next = s + 1
          prob = strategy (CarCount next)
       in (next, p * prob, col || (prob == 0.0))
    step (s, p, col) Dec =
      (max 0 (s - 1), p, col)

-- | Custom equivalence operator to check if two simulation results match.
(~=) :: [(CarCount, Double)] -> [(CarCount, Double)] -> Bool
[] ~= [] = True
[(CarCount s1, p1)] ~= [(CarCount s2, p2)] =
  (s1 == s2) && (abs (p1 - p2) < 1e-6)
_ ~= _ = False

-- ==========================================
-- Hover Dam Tests (parameterised by strategy)
-- ==========================================

-- | Runs a scenario defined by a list of 'Action's and checks
-- it matches the Oracle's prediction for the given strategy.
damTest :: DamStrategy -> String -> [Action] -> TestTree
damTest strategy name actions = testCase name $
  let dist = runSubdist (foldl (>>=) damOpens (map (actionToMove strategy) actions))
   in dist @?= calculateExpectedResult strategy actions 0

-- | Generic property that verifies ANY random sequence of moves for a given strategy.
prop_damRandomPath :: DamStrategy -> Property
prop_damRandomPath strategy = property $ do
  actions <- genRandomActions
  let expected = calculateExpectedResult strategy actions 0
      moves = map (actionToMove strategy) actions
      result = runSubdist (foldl (>>=) damOpens moves)
  return $ result ~= expected

-- | All scenario tests for a given strategy.
hoverDamScenarioTests :: String -> DamStrategy -> TestTree
hoverDamScenarioTests name strategy =
  testGroup
    name
    [ damTest strategy "Safe Scenario: 2 enter, 2 leave"            [Inc, Inc, Dec, Dec],
      damTest strategy "Bifurcation Scenario: Reaching Capacity"    [Inc, Inc, Inc],
      damTest strategy "Collapse Scenario: 50% chance at threshold" [Inc, Inc, Inc, Inc],
      damTest strategy "Collapse Scenario: 25% chance at threshold" [Inc, Inc, Inc, Inc, Dec, Inc],
      damTest strategy "Total Collapse: Exceeding threshold"        [Inc, Inc, Inc, Inc, Inc],
      testProperty "Random Arbitrary Path Verification" (prop_damRandomPath strategy)
    ]

<<<<<<< HEAD
=======
prop_damManualProbVerify :: Property
prop_damManualProbVerify = property $ do
  numSteps <- choose (0, 30 :: Int)
  let CarCount thresh = damCollapseThreshold
  actions <- take numSteps <$> genReflectingActions 0 (thresh + 1) 0
  result <- genHoverDamWalk 0 (pure actions)
  let moves = map actionToMove actions
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

>>>>>>> f82d4f1 (Refactor: Unified RandomWalk analysis with streaming snapshots and added genReflectingActions)
-- ==========================================
-- Master Test Tree
-- ==========================================

hoverDamTests :: TestTree
hoverDamTests =
  testGroup
    "Hover Dam (Subdist)"
    [ hoverDamScenarioTests "Step Strategy" strategyStep,
      hoverDamScenarioTests "Two-Step Strategy" strategyTwoStep,
      hoverDamScenarioTests "Linear Strategy" strategyLinear
    ]
