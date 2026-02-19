module Lambda (
  safeHead, fact, addMaybes, 
  applyTwice, safeDiv, MyMaybe(..),
  MyReader(..), runMyReader) 
  where 

import Data.Function (fix)
import Control.Applicative (liftA2)
import Lambda.Functor

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

