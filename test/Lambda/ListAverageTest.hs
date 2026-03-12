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
        [ testCase "sumCase calculates the sum of elements" $
            sumCase [1.0, 2.0, 3.0, 4.0] @?= 10.0,
          testCase "lenFold calculates the length of the list" $
            lenFold [1.0, 2.0, 3.0, 4.0] @?= 4.0
        ],
      testGroup
        "QuickCheck"
        [ testProperty "sumCase equals sumFold" $ \xs ->
            let diff = abs (sumCase (xs :: [Double]) - sumFold xs)
            in diff < 1e-9,
          testProperty "sumCase equals sum" $ \xs ->
            let diff = abs (sumCase (xs :: [Double]) - sum xs)
            in diff < 1e-9,
          testProperty "lenFold equals lenCase" $ \xs ->
            lenFold (xs :: [Double]) === lenCase xs,
          testProperty "lenFold equals length" $ \xs ->
            lenFold xs === fromIntegral (length (xs :: [Double])),
          testProperty "sumAndCount equals (sum, length)" $ \xs ->
            sumAndCount xs === (sum (xs :: [Double]), fromIntegral (length xs)),
          testProperty "average equals sum / length" $ \xs ->
            not (null xs) ==> average (xs :: [Double]) === sum xs / fromIntegral (length xs),
          testProperty "sumAndCount variants are equivalent" $ \xs ->
            sumAndCount (xs :: [Double]) === sumAndCountMonadic xs
        ]
    ]
