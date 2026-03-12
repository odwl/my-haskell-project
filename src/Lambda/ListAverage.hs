module Lambda.ListAverage where

import Control.Monad.State
import Data.Functor.Const (Const (..), getConst)
import Data.Monoid (Sum (..), getSum)

-- import Data.Bifunctor (bimap)

-- | Returns the sum of all elements using pattern matching
{-# ANN sumCase "HLint: ignore Use foldr" #-}
{-# ANN sumCase "HLint: ignore Use sum" #-}
sumCase :: [Double] -> Double
sumCase [] = 0
sumCase (x : xs) = x + sumCase xs

-- | Returns the sum of all elements using fold
{-# ANN sumFold "HLint: ignore Use sum" #-}
sumFold :: [Double] -> Double
sumFold = foldl (+) 0

-- | Returns the sum of all elements using monoid map
sumMonoid :: [Double] -> Double
sumMonoid = getSum . foldMap Sum

-- | Returns the sum using Applicative '<*>' on Const (Sum Double)
sumApplicative :: [Double] -> Double
sumApplicative = getSum . getConst . traverse (Const . Sum)

-- | Returns the length of the list using fold
lenFold :: [Double] -> Double
lenFold = foldl (\acc _ -> acc + 1) 0

-- | Returns the length of the list using pattern matching
lenCase :: [Double] -> Double
lenCase [] = 0
lenCase (_ : xs) = 1 + lenCase xs

-- | Returns the sum of all elements
mySum :: [Double] -> Double
mySum = sum

-- | Returns the count of all elements
myCount :: [Double] -> Double
myCount = foldl (\acc _ -> acc + 1) 0

-- | Tells the writer to increment the count by 1.
increment :: Double -> Sum Double
increment _ = Sum 1

-- | Uses the Monoid instance of Sum to accumulate
myCountMonoid :: [Double] -> Double
myCountMonoid xs = getSum $ foldMap (\_ -> Sum 1) xs

-- | Uses the Applicative instance of Const (which relies on <*>) to accumulate
myCountApplicative :: [Double] -> Double
myCountApplicative xs = getSum $ getConst $ traverse (\_ -> Const (Sum 1)) xs

-- -- | Computes sum and count in a single traversal
sumAndCount :: [Double] -> (Double, Double)
sumAndCount = foldl (\(s, c) x -> (s + x, c + 1)) (0, 0)

-- sumAndCount = foldl (flip ((flip bimap (+ 1)) . (+))) (0, 0)
-- sumAndCount = foldl (\acc x -> bimap (+ x) (+ 1) acc) (0, 0)

-- -- -- | Use the result of sumAndCount to find the average
average :: [Double] -> Double
average = uncurry (/) . sumAndCount

addNumber :: Double -> State (Double, Double) ()
addNumber x = modify (\(s, c) -> (s + x, c + 1))

sumAndCountMonadic :: [Double] -> (Double, Double)
sumAndCountMonadic xs = execState (mapM_ addNumber xs) (0, 0)
