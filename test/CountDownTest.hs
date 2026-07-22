{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module CountDownTest (countDownTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding (Positive)
import CountDown
import Data.List (sort)
import Data.Maybe (fromJust)
import Control.Arrow ((***), (>>>))

p :: Int -> Positive
p = fromJust . mkPositive

app :: Op -> Expr -> Expr -> Expr
app op l r = fromJust (mkApp op l r)

v2, v3, v4, v5, v6 :: Expr
v2 = Val (p 2)
v3 = Val (p 3)
v4 = Val (p 4)
v5 = Val (p 5)
v6 = Val (p 6)

nestedExpr :: Expr
nestedExpr = app Mul v4 (app Add v2 v3)

instance Arbitrary Positive where
  arbitrary = p <$> frequency [(1, return 1), (9, choose (2, 1000))]

countDownTests :: TestTree
countDownTests = testGroup "CountDown Tests"
  [ testGroup "Unit Tests"
      [ testCase "apply Add" $ apply Add (p 2) (p 3) @?= p 5
      , testCase "apply Sub" $ apply Sub (p 5) (p 2) @?= p 3
      , testCase "apply Mul" $ apply Mul (p 3) (p 4) @?= p 12
      , testCase "apply Div" $ apply Div (p 6) (p 2) @?= p 3
      , testCase "valid Sub" $ valid Sub (p 5) (p 2) @?= True
      , testCase "invalid Sub" $ valid Sub (p 2) (p 5) @?= False
      , testCase "invalid Mul by 1" $ valid Mul one (p 5) @?= False
      , testCase "invalid Div by 1" $ valid Div (p 5) one @?= False
      , testCase "show Expr Val" $ show v5 @?= "5"
      , testCase "show Expr simple" $ show (app Add v2 v3) @?= "2+3"
      , testCase "show Expr nested" $ show nestedExpr @?= "4*(2+3)"
      
      -- Test values extraction
      , testCase "values extraction" $ 
          values nestedExpr @?= [p 4, p 2, p 3]

      -- Test eval (Pure)
      , testCase "eval valid Expr" $ 
          eval nestedExpr @?= p 20
          
      -- Test choices (generation of subsets and permutations)
      , testCase "choices of empty list" $ 
          choices ([] :: [Int]) @?= [[]]
      , testCase "choices of [1, 2]" $ 
          sort (choices ([1, 2] :: [Int])) @?= sort [[], [1], [2], [1, 2], [2, 1]]
          
      -- Test split
      , testCase "split empty list" $
          split ([] :: [Int]) @?= []
      , testCase "split singleton list" $
          split ([1] :: [Int]) @?= []
      , testCase "split [1, 2]" $
          split ([1, 2] :: [Int]) @?= [([1], [2])]
      , testCase "split [1, 2, 3]" $
          split ([1, 2, 3] :: [Int]) @?= [([1], [2, 3]), ([1, 2], [3])]

      -- Test combine
      , testCase "combine 2 and 3 (only Add and Mul valid due to symmetry pruning)" $
          combine v2 v3 @?= [app Add v2 v3, app Mul v2 v3]
      , testCase "combine 3 and 2 (only Sub valid)" $
          combine v3 v2 @?= [app Sub v3 v2]
      , testCase "combine 6 and 2 (Sub and Div valid)" $
          combine v6 v2 @?= [app Sub v6 v2, app Div v6 v2]

      -- Test solution checking
      , testCase "solution valid" $ 
          solution nestedExpr [p 2, p 3, p 4, p 5] (p 20) @?= True
      , testCase "solution invalid (missing number from source)" $ 
          solution nestedExpr [p 2, p 3, p 5] (p 20) @?= False
      , testCase "solution invalid (wrong target value)" $ 
          solution nestedExpr [p 2, p 3, p 4, p 5] (p 25) @?= False
      ]
  , testGroup "QuickCheck Properties"
      [ testProperty "Add is valid iff x <= y" prop_addValid
      , testProperty "Sub is valid iff x > y" prop_subValid
      , testProperty "Mul is valid iff x <= y and x /= 1" prop_mulValid
      , testProperty "valid Div when x is multiple of y" prop_divValid
      , testProperty "invalid Div when x is not a multiple of y" prop_divInvalid
      , testProperty "split halves always concatenate to original list (correct length)" prop_splitPreserves
      , testProperty "split halves are never empty" prop_splitNonEmpty
      ]
  ]
  where
    prop_addValid x yVal = valid Add x yVal === (x <= yVal)
    prop_subValid x yVal = valid Sub x yVal === (x > yVal)
    prop_mulValid x yVal = valid Mul x yVal === (x <= yVal && x /= one)
    prop_divValid kVal yVal = valid Div (kVal .* yVal) yVal === (yVal /= one)
    prop_divInvalid k y r = 
      let divisor = y .+ r
          dividend = k .* divisor .+ r
       in valid Div dividend divisor === False

    prop_splitPreserves (xs :: [Int]) = 
      fmap (uncurry (++)) (split xs) === replicate (max 0 (length xs - 1)) xs
      
    prop_splitNonEmpty (xs :: [Int]) = 
      all (null *** null >>> (== (False, False))) $ split xs
