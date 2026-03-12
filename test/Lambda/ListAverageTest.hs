module Lambda.ListAverageTest (listAverageTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Lambda.ListAverage

listAverageTests :: TestTree
listAverageTests =
  testGroup
    "ListAverage Tests"
    [ testGroup
        "Unit Tests"
        [ testCase "func1 calculates the sum of elements" $
            func1 [1.0, 2.0, 3.0, 4.0] @?= 10.0,
          testCase "func2 calculates the length of the list" $
            func2 [1.0, 2.0, 3.0, 4.0] @?= 4.0
        ],
      testGroup
        "QuickCheck"
        [ testProperty "func1 equals sum" $ \xs ->
            func1 xs === sum (xs :: [Double]),
          testProperty "func2 equals length" $ \xs ->
            func2 xs === fromIntegral (length (xs :: [Double])),
          testProperty "sumAndCount equals (sum, length)" $ \xs ->
            sumAndCount xs === (sum (xs :: [Double]), fromIntegral (length xs)),
          testProperty "average equals sum / length" $ \xs ->
            not (null xs) ==> average (xs :: [Double]) === sum xs / fromIntegral (length xs),
          testProperty "sumAndCount variants are equivalent" $ \xs ->
            sumAndCount (xs :: [Double]) === sumAndCountMonadic xs
        ]
    ]
