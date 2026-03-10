module Exercism.ClockTest (clockTests) where

import Exercism.Clock (addDelta, fromHourMin, toString)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

clockTests :: TestTree
clockTests =
  testGroup
    "Clock"
    [ testGroup "Creation & Normalization" creationTests,
      testGroup "String Representation" toStringTests,
      testGroup "Add Delta (Normalization)" addDeltaTests,
      testGroup "Equality" equalityTests
    ]

creationTests :: [TestTree]
creationTests =
  [ testCase "on the hour" $
      fromHourMin 8 0 @?= fromHourMin 8 0,
    testCase "past the hour" $
      fromHourMin 11 9 @?= fromHourMin 11 9,
    testCase "valid boundary: 23:59" $
      fromHourMin 23 59 @?= fromHourMin 23 59,
    testCase "valid boundary: 00:00" $
      fromHourMin 0 0 @?= fromHourMin 0 0,
    testCase "normalization: midnight is 24 hours" $
      fromHourMin 24 0 @?= fromHourMin 0 0,
    testCase "normalization: hour rolls over" $
      fromHourMin 25 0 @?= fromHourMin 1 0,
    testCase "normalization: negative hour" $
      fromHourMin (-1) 15 @?= fromHourMin 23 15,
    testCase "normalization: sixty minutes" $
      fromHourMin 1 60 @?= fromHourMin 2 0,
    testCase "normalization: negative minutes" $
      fromHourMin 1 (-40) @?= fromHourMin 0 20
  ]

toStringTests :: [TestTree]
toStringTests =
  [ testCase "format: 08:00" $
      toString (fromHourMin 8 0) @?= "08:00",
    testCase "format: 11:09" $
      toString (fromHourMin 11 9) @?= "11:09",
    testCase "format: normalized 24:00" $
      toString (fromHourMin 24 0) @?= "00:00"
  ]

addDeltaTests :: [TestTree]
addDeltaTests =
  [ testCase "add minutes" $
      addDelta 0 3 (fromHourMin 10 0) @?= fromHourMin 10 3,
    testCase "add to next hour" $
      addDelta 0 40 (fromHourMin 0 45) @?= fromHourMin 1 25,
    testCase "wrap around midnight" $
      addDelta 0 1 (fromHourMin 23 59) @?= fromHourMin 0 0,
    testCase "subtract 1 min from 00:00" $
      addDelta 0 (-1) (fromHourMin 0 0) @?= fromHourMin 23 59,
    testCase "subtract more than two hours with negative hours" $
      addDelta (-2) (-40) (fromHourMin 10 0) @?= fromHourMin 7 20,
    testCase "large positive delta (100 days)" $
      addDelta 0 (100 * 24 * 60) (fromHourMin 10 0) @?= fromHourMin 10 0,
    testCase "large negative delta (-100 days)" $
      addDelta 0 (-(100 * 24 * 60)) (fromHourMin 10 0) @?= fromHourMin 10 0,
    testCase "add negative hour and minute delta" $
      addDelta (-25) (-160) (fromHourMin 10 0) @?= fromHourMin 6 20
  ]

equalityTests :: [TestTree]
equalityTests =
  [ testCase "clocks with same time" $
      fromHourMin 15 37 @?= fromHourMin 15 37,
    testCase "clocks a minute apart" $
      (fromHourMin 15 36 == fromHourMin 15 37) @?= False,
    testCase "different non-normalized clocks are equal if they represent same time" $
      fromHourMin 25 0 == fromHourMin 24 0 @?= False, -- 01:00 vs 00:00
    testCase "clocks that normalize to same time are equal" $
      fromHourMin 24 0 == fromHourMin 0 0 @?= True
  ]
