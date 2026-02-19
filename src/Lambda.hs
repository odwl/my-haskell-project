module Lambda (
  safeHead, fact, addMaybes, 
  applyTwice, safeDiv, MyMaybe(..),
  MyReader, mkReader, runMyReader) 
  where 

import Data.Function (fix)
import Control.Applicative (liftA2)

-- factorial using fix.
fact :: Int -> Int
fact = fix step
  where step f n = if n == 0 then 1 else n * f (n - 1)

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

addMaybes :: Maybe Int -> Maybe Int -> Maybe Int
addMaybes = liftA2 (+)

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (div x y)

data MyMaybe a = MyNothing | MyJust a deriving (Show, Eq)
instance Functor MyMaybe where
  fmap _ MyNothing = MyNothing
  fmap f (MyJust x) = MyJust (f x)

newtype MyReader a b = Ploc { unwrap :: a -> b }
instance Functor (MyReader c) where 
  fmap f (Ploc g) = Ploc (f.g)

-- The "Smart Constructor"
-- Outside modules call this, and it secretly uses Ploc under the hood
mkReader :: (a -> b) -> MyReader a b
mkReader f = Ploc f

runMyReader :: MyReader a b -> a -> b
runMyReader = unwrap