module Lambda.ListAverage where

import Control.Monad.State


-- import Data.Bifunctor (bimap)

-- | Returns the sum of all elements
func1 :: [Double] -> Double
func1 = foldl (+) 0  

-- | Placeholder for the second function
func2 :: [Double] -> Double
func2 = foldl (\acc _ -> acc + 1) 0

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
sumAndCountMonadic xs = execState (mapM_ addNumber xs) (0,0)
