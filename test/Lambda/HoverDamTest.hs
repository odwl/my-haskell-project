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

-- | Generates a random sequence of up to 50 actions for simulation testing.
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
   in filter (\(_, q) -> q > 1e-12) [(CarCount f, p) | not col]
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
damTest strategy name actions =
  testCase name $
    let dist = runSubdist (foldl (>>=) damOpens (map (actionToMove strategy) actions))
        expected = calculateExpectedResult strategy actions 0
     in assertBool ("Results differ: " ++ show dist ++ " != " ++ show expected) (dist ~= expected)

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
    [ damTest strategy "Safe Scenario: 2 enter, 2 leave" [Inc, Inc, Dec, Dec],
      damTest strategy "Bifurcation Scenario: Reaching Capacity" [Inc, Inc, Inc],
      damTest strategy "Collapse Scenario: 50% chance at threshold" [Inc, Inc, Inc, Inc],
      damTest strategy "Collapse Scenario: 25% chance at threshold" [Inc, Inc, Inc, Inc, Dec, Inc],
      damTest strategy "Total Collapse: Exceeding threshold" [Inc, Inc, Inc, Inc, Inc],
      testProperty "Random Arbitrary Path Verification" (prop_damRandomPath strategy)
    ]

-- ==========================================
-- Master Test Tree
-- ==========================================

-- | The master test tree for Hover Dam simulation logic.
hoverDamTests :: TestTree
hoverDamTests =
  testGroup
    "Hover Dam (Subdist)"
    [ hoverDamScenarioTests "Step Strategy" strategyStep,
      hoverDamScenarioTests "Two-Step Strategy" strategyTwoStep,
      hoverDamScenarioTests "Linear Strategy" strategyLinear
    ]
