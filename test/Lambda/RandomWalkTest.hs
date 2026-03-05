module Lambda.RandomWalkTest where

import qualified Data.Map as Map
import Lambda.RandomWalk (Action (..), RandomWalk (..), analyzePath, applyAction, applyReflectingBounds, genInfiniteActions)
import Test.Tasty
import Test.Tasty.HUnit
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
   in forAll genInfiniteActions $ \infActions ->
        let actions = take numSteps $ applyReflectingBounds fixedMin fixedMax start infActions
            trajectory = scanl (flip applyAction) start actions
         in counterexample ("Trajectory: " ++ show trajectory) $
              all (\v -> v >= fixedMin && v <= fixedMax) trajectory

-- | Property to verify that analyzePath correctly counts the number of visits.
prop_analyzePathCounts :: Int -> Int -> Int -> Property
prop_analyzePathCounts b1 b2 s =
  let minB = min b1 b2
      maxB = max b1 b2
      (fixedMin, fixedMax) = if minB == maxB then (minB, minB + 1) else (minB, maxB)
      start = max fixedMin (min fixedMax s)
      numSteps = 100
   in forAll genInfiniteActions $ \infActions ->
        let actions = take numSteps $ applyReflectingBounds fixedMin fixedMax start infActions
            rwalk = RandomWalk start (pure actions)
         in forAll (analyzePath rwalk) $ \visits ->
              sum (Map.elems visits) === numSteps + 1

-- | Simple unit test for analyzePath illustration.
test_analyzePathSimple :: TestTree
test_analyzePathSimple = testCase "Analyze path simple example" $ do
  let start = 0
      actions = [Inc, Dec, Inc] -- 0 -> 1 -> 0 -> 1
      rwalk = RandomWalk start (pure actions)
  visits <- generate (analyzePath rwalk)
  let expected = Map.fromList [(0, 2), (1, 2)]
  visits @?= expected

randomWalkTests :: TestTree
randomWalkTests =
  testGroup
    "Random Walk Generator"
    [ testProperty "Stay within boundaries" prop_randomWalkStayWithinBoundaries,
      testProperty "Analyze path correctly counts visits" prop_analyzePathCounts,
      test_analyzePathSimple
    ]
