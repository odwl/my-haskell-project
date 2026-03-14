{-# LANGUAGE EmptyCase #-}

module Lambda.ListAverage where

import Control.Monad (foldM, (>=>))
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

-----------------------------------
-- Minimum Void
-----------------------------------
data Never

absurd :: Never -> a
absurd v = case v of {}

vacuous :: (Functor f) => f Never -> f a
vacuous = fmap absurd

collapse :: Either Never a -> a
collapse (Left v) = absurd v
collapse (Right x) = x

-----------------------------------
-- Minimum Foldable
-----------------------------------
-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m

data VoidFoldable a

instance Foldable VoidFoldable where
  foldMap _ v = case v of {}

-- My sum foldable.
newtype MySum a = MySum a deriving (Eq, Show)

instance Foldable MySum where
  foldMap f (MySum a) = f a

-- instance Functor MySum where
--   fmap f (MySum a) = MySum (f a)

-- | Returns the sum of all elements using fold
-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
{-# ANN sumFold "HLint: ignore Use sum" #-}
sumFold :: [Double] -> Double
sumFold = foldl (+) 0

-- | Returns the sum of all elements using monoid map
-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
-- Sum :: a -> Sum a
sumMonoid :: [Double] -> Double
sumMonoid = getSum . foldMap Sum

-- | Returns the sum using Applicative '<*>' on Const (Sum Double)
-- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
-- Const :: a -> Const a b
-- getConst :: Const a b -> a
sumApplicative :: [Double] -> Double
sumApplicative = getSum . getConst . traverse (Const . Sum)

-- | Returns the sum of all elements using foldl and Monad bind (>>=) on Sum
-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
sumMonad :: [Double] -> Double
sumMonad = getSum . foldl (\acc x -> acc >>= \a -> Sum (a + x)) (Sum 0)

-- | Returns the sum of all elements using foldM on Sum
-- foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
sumFoldM :: [Double] -> Double
sumFoldM = getSum . foldM (\acc x -> return (acc + x)) 0

-- | Returns the sum of all elements using foldr and Kleisli composition (>=>)
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
sumKleisli :: [Double] -> Double
sumKleisli xs = getSum $ foldr ((>=>) . (\x acc -> Sum (acc + x))) return xs 0

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
