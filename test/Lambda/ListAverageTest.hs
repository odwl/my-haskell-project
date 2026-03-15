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
        [ testCase "sumCase calculates the sum of elements" $
            sumCase [1.0, 2.0, 3.0, 4.0] @?= 10.0,
          testCase "lenFoldl calculates the length of the list" $
            lenFoldl [1.0, 2.0, 3.0, 4.0] @?= 4.0,
          testCase "lenFoldr calculates the length of the list" $
            lenFoldr [1.0, 2.0, 3.0, 4.0] @?= 4.0,
          testCase "mySum calculates the sum of elements" $
            mySum [1.0, 2.0, 3.0, 4.0] @?= 10.0,
          testCase "myCount calculates the length of the list" $
            myCount [1.0, 2.0, 3.0, 4.0] @?= 4.0
        ],
      testGroup
        "QuickCheck"
        [ testProperty "sum functions (Case, Fold, Monoid, Applicative, Monad, Kleisli) are invariant" $ \xs ->
            let input = xs :: [Double]
                vCase = sumCase input
                vFoldl = sumFoldl input
                vFoldr = sumFoldr input
                vMonoid = sumMonoid input
                vApplicative = sumApplicative input
                vMonad = sumMonad input
                vFoldM = sumFoldM input
                vKleisli = sumKleisli input
                diffCaseFoldl = abs (vCase - vFoldl)
                diffCaseFoldr = abs (vCase - vFoldr)
                diffCaseMonoid = abs (vCase - vMonoid)
                diffCaseApplicative = abs (vCase - vApplicative)
                diffCaseMonad = abs (vCase - vMonad)
                diffCaseFoldM = abs (vCase - vFoldM)
                diffCaseKleisli = abs (vCase - vKleisli)
             in diffCaseFoldl < 1e-9 && diffCaseFoldr < 1e-9 && diffCaseMonoid < 1e-9 && diffCaseApplicative < 1e-9 && diffCaseMonad < 1e-9 && diffCaseFoldM < 1e-9 && diffCaseKleisli < 1e-9,
          testProperty "sumCase equals sum" $ \xs ->
            let diff = abs (sumCase (xs :: [Double]) - sum xs)
             in diff < 1e-9,

          testProperty "lenFoldl equals lenCase" $ \xs ->
            lenFoldl (xs :: [Double]) === lenCase xs,
          testProperty "lenFoldr equals lenCase" $ \xs ->
            lenFoldr (xs :: [Double]) === lenCase xs,
          testProperty "lenFoldl equals length" $ \xs ->
            lenFoldl xs === fromIntegral (length (xs :: [Double])),
          testProperty "lenFoldr equals length" $ \xs ->
            lenFoldr xs === fromIntegral (length (xs :: [Double])),
          testProperty "mySum equals sum" $ \xs ->
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
