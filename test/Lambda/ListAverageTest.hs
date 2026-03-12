module Lambda.ListAverageTest (listAverageTests) where

import Lambda.ListAverage
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

listAverageTests :: TestTree
listAverageTests =
  testGroup
    "ListAverage Tests"
    [ testGroup
        "Unit Tests"
        [ testCase "mySum calculates the sum of elements" $
            mySum [1.0, 2.0, 3.0, 4.0] @?= 10.0,
          testCase "myCount calculates the length of the list" $
            myCount [1.0, 2.0, 3.0, 4.0] @?= 4.0
        ],
      testGroup
        "QuickCheck"
        [ testProperty "mySum equals sum" $ \xs ->
            mySum xs === sum (xs :: [Double]),
          testProperty "myCount equals length" $ \xs ->
            myCount xs === fromIntegral (length (xs :: [Double])),
          testProperty "myCountMonoid equals myCount" $ \xs ->
            myCountMonoid (xs :: [Double]) === myCount xs,
          testProperty "myCountApplicative equals myCount" $ \xs ->
            myCountApplicative (xs :: [Double]) === myCount xs,
          testProperty "sumAndCount equals (sum, length)" $ \xs ->
            sumAndCount xs === (sum (xs :: [Double]), fromIntegral (length xs)),
          testProperty "average equals sum / length" $ \xs ->
            not (null xs) ==> average (xs :: [Double]) === sum xs / fromIntegral (length xs),
          testProperty "sumAndCount variants are equivalent" $ \xs ->
            sumAndCount (xs :: [Double]) === sumAndCountMonadic xs
        ]
    ]
