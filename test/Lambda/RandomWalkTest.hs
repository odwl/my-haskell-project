module Lambda.RandomWalkTest where

import Control.Monad (foldM)
import Test.Tasty
import Test.Tasty.QuickCheck

data Action = Inc | Dec deriving (Show, Eq)

applyAction :: Action -> Int -> Int
applyAction Inc n = n + 1
applyAction Dec n = n - 1

-- | Generates a list of actions (Inc or Dec) that, when applied sequentially
-- starting from 'start', keep the value within [minB, maxB].
--
-- Arguments:
-- 1. minB: Lower boundary (inclusive)
-- 2. maxB: Upper boundary (inclusive)
-- 3. start: Initial value
-- 4. numSteps: Number of actions to generate
genBoundedActions :: Int -> Int -> Int -> Int -> Gen [Action]
genBoundedActions minB maxB start numSteps = do
  (_, actions) <- foldM next (start, []) [1 .. numSteps]
  return (reverse actions)
  where
    next (curr, acc) _ = do
      let canInc = curr < maxB
          canDec = curr > minB

      action <- case (canInc, canDec) of
        (True, True) -> elements [Inc, Dec]
        (True, False) -> return Inc
        (False, True) -> return Dec
        (False, False) -> return Inc -- Should not happen if minB < maxB
      return (applyAction action curr, action : acc)

-- | Property to verify that the generated walk stays within boundaries.
prop_randomWalkStayWithinBoundaries :: Int -> Int -> Int -> Property
prop_randomWalkStayWithinBoundaries b1 b2 s =
  -- Ensure non-empty range for simplicity in this test
  let minB = min b1 b2
      maxB = max b1 b2
      -- If minB == maxB, the only way is to stay at that value
      -- But let's just make sure minB < maxB for a more interesting test
      (fixedMin, fixedMax) = if minB == maxB then (minB, minB + 1) else (minB, maxB)
      start = max fixedMin (min fixedMax s)
      numSteps = 100
   in forAll (genBoundedActions fixedMin fixedMax start numSteps) $ \actions ->
        let trajectory = scanl (flip applyAction) start actions
         in counterexample ("Trajectory: " ++ show trajectory) $
              all (\v -> v >= fixedMin && v <= fixedMax) trajectory

randomWalkTests :: TestTree
randomWalkTests =
  testGroup
    "Random Walk Generator"
    [ testProperty "Stay within boundaries" prop_randomWalkStayWithinBoundaries
    ]
