module Lambda.RandomWalkTest where

import Lambda.FunctorTestUtils (applyAction, genBoundedActions)
import Test.Tasty
import Test.Tasty.QuickCheck

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
