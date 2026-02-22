module Lambda
  ( safeHead,
    fact,
    addMaybes,
    applyTwice,
    safeDiv,
  )
where

import Data.Function (fix)

-- factorial using fix.
fact :: Int -> Int
fact = fix step
  where
    step f n = if n == 0 then 1 else n * f (n - 1)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

addMaybes :: Maybe Int -> Maybe Int -> Maybe Int
addMaybes = liftA2 (+)

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (div x y)