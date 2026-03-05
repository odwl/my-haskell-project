module Lambda.RandomWalkTest where

import qualified Data.Map as Map
import Lambda.RandomWalk (Action (..), RandomWalk (..), applyAction, applyReflectingBounds, genInfiniteActions, pathSnapshots)
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
   in forAll (applyReflectingBounds fixedMin fixedMax start <$> genInfiniteActions) $ \infActions ->
        let actions = take numSteps infActions
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
   in forAll (applyReflectingBounds fixedMin fixedMax start <$> genInfiniteActions) $ \infActions ->
        let actions = take numSteps infActions
            rwalk = RandomWalk start (pure actions)
         in forAll (last <$> pathSnapshots rwalk) $ \visits ->
              sum (Map.elems visits) === numSteps + 1

-- | Simple unit test for analyzePath illustration.
test_analyzePathSimple :: TestTree
test_analyzePathSimple = testCase "Analyze path simple example" $ do
  let start = 0
      actions = [Inc, Dec, Inc] -- 0 -> 1 -> 0 -> 1
      rwalk = RandomWalk start (pure actions)
  visits <- generate (last <$> pathSnapshots rwalk)
  let expected = Map.fromList [(0, 2), (1, 2)]
  visits @?= expected

-- | Unit test for pathSnapshots.
test_pathSnapshotsSimple :: TestTree
test_pathSnapshotsSimple = testCase "pathSnapshots simple example" $ do
  let start = 0
      actions = [Inc, Dec] -- 0 -> 1 -> 0
      rwalk = RandomWalk start (pure actions)
  snapshots <- generate (pathSnapshots rwalk)
  let expected =
        [ Map.fromList [(0, 1)], -- Step 0: start
          Map.fromList [(0, 1), (1, 1)], -- Step 1: 0 -> 1
          Map.fromList [(0, 2), (1, 1)] -- Step 2: 1 -> 0
        ]
  take 3 snapshots @?= expected

-- | Unit test for applyReflectingBounds.
test_applyReflectingBoundsSimple :: TestTree
test_applyReflectingBoundsSimple = testCase "applyReflectingBounds simple example" $ do
  let start = 1
      minB = 0
      maxB = 2
      -- 1 -> 2 -> 2 (reflects to 1) -> 0 -> 0 (reflects to 1)
      -- step 1: curr=1, action=Inc -> next=2, canInc=True -> boundedAction=Inc -> nextS=2
      -- step 2: curr=2, action=Inc -> next=3, canInc=False, canDec=True -> boundedAction=Dec -> nextS=1
      -- step 3: curr=1, action=Dec -> next=0, canDec=True -> boundedAction=Dec -> nextS=0
      -- step 4: curr=0, action=Dec -> next=-1, canInc=True, canDec=False -> boundedAction=Inc -> nextS=1
      actions = [Inc, Inc, Dec, Dec]
      reflected = applyReflectingBounds minB maxB start actions
  reflected @?= [Inc, Dec, Dec, Inc]

randomWalkTests :: TestTree
randomWalkTests =
  testGroup
    "Random Walk Generator"
    [ testProperty "Stay within boundaries" prop_randomWalkStayWithinBoundaries,
      testProperty "Analyze path correctly counts visits" prop_analyzePathCounts,
      test_analyzePathSimple,
      test_pathSnapshotsSimple,
      test_applyReflectingBoundsSimple
    ]
