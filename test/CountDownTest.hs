{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module CountDownTest (countDownTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding (Positive)
import CountDown
import Data.List (sort, nub)
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
          choices ([] :: [Positive]) @?= [[]]
      , testCase "choices of [1, 2]" $ 
          sort (choices ([p 1, p 2] :: [Positive])) @?= sort [[], [p 1], [p 2], [p 1, p 2], [p 2, p 1]]
          
      -- Test split
      , testCase "split empty list" $
          split ([] :: [Positive]) @?= []
      , testCase "split singleton list" $
          split ([p 1] :: [Positive]) @?= []
      , testCase "split [1, 2]" $
          split ([p 1, p 2] :: [Positive]) @?= [([p 1], [p 2])]
      , testCase "split [1, 2, 3]" $
          split ([p 1, p 2, p 3] :: [Positive]) @?= [([p 1], [p 2, p 3]), ([p 1, p 2], [p 3])]

      -- Test combine
      , testCase "combine 2 and 3 (only Add and Mul valid due to symmetry pruning)" $
          combine v2 v3 @?= [app Add v2 v3, app Mul v2 v3]
      , testCase "combine 3 and 2 (only Sub valid)" $
          combine v3 v2 @?= [app Sub v3 v2]
      , testCase "combine 6 and 2 (Sub and Div valid)" $
          combine v6 v2 @?= [app Sub v6 v2, app Div v6 v2]

      -- Test exprs
      , testCase "exprs empty list" $
          exprs [] @?= []
      , testCase "exprs singleton [2]" $
          exprs [p 2] @?= [v2]
      , testCase "exprs [2, 3]" $
          exprs [p 2, p 3] @?= [app Add v2 v3, app Mul v2 v3]
      , testCase "exprs [3, 2]" $
          exprs [p 3, p 2] @?= [app Sub v3 v2]



      -- Test solve
      , testCase "solve [2, 3] for target 5" $
          solve [p 2, p 3] (p 5) @?= [app Add v2 v3]
      , testCase "solve [2, 3] for target 6" $
          solve [p 2, p 3] (p 6) @?= [app Mul v2 v3]
      , testCase "solve [2, 3] for impossible target 4" $
          solve [p 2, p 3] (p 4) @?= []
      , testCase "solve Countdown benchmark [1, 3, 7, 10, 25, 50] for target 765 is non-empty" $
          not (null (solve [p 1, p 3, p 7, p 10, p 25, p 50] (p 765))) @?= True
      ]
  , testGroup "QuickCheck Properties"
      [ testProperty "Add is valid iff x <= y" prop_addValid
      , testProperty "Sub is valid iff x > y" prop_subValid
      , testProperty "Mul is valid iff x <= y and x /= 1" prop_mulValid
      , testProperty "valid Div when x is multiple of y" prop_divValid
      , testProperty "invalid Div when x is not a multiple of y" prop_divInvalid
      , testProperty "split halves always concatenate to original list (correct length)" prop_splitPreserves
      , testProperty "split halves are never empty" prop_splitNonEmpty
      , testProperty "exprs preserves exact sequence of input values" prop_exprsPreservesValues
      , testProperty "exprs never produces duplicate trees" prop_exprsUnique
      , testProperty "exprs length never exceeds Catalan upper bound" prop_exprsWithinBound
      , testProperty "every solution returned by solve evaluates to target" prop_solveMatchesTarget
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

    prop_splitPreserves (xs :: [Positive]) = 
      fmap (uncurry (++)) (split xs) === replicate (max 0 (length xs - 1)) xs
      
    prop_splitNonEmpty (xs :: [Positive]) = 
      all (null *** null >>> (== (False, False))) $ split xs

    prop_exprsPreservesValues (ns :: [Positive]) =
      map values (exprs ns) === replicate (length (exprs ns)) ns

    prop_exprsUnique (ns :: [Positive]) =
      length es === length (nub es)
      where
        es = exprs ns

    prop_exprsWithinBound (ns :: [Positive]) =
      length (exprs ns) <= maxTrees
      where
        k = length ns
        maxTrees = if k == 0 then 0 else catalan (k - 1) * (4 ^ (k - 1))

    prop_solveMatchesTarget (ns :: [Positive]) (target :: Positive) =
      all (\e -> eval e == target) (solve ns target)

--------------------------------------------------------------------------------
-- Mathematical Helpers
--------------------------------------------------------------------------------

-- Memoized infinite list of Catalan numbers: [1, 1, 2, 5, 14, 42, 132, ...]
catalans :: [Int]
catalans = 1 : [sum (zipWith (*) catalans (reverse (take n catalans))) | n <- [1..]]

catalan :: Int -> Int
catalan n = catalans !! n
