{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Lambda.Functor
  ( MyMaybe (..),
    MyReader (..),
    runMyReader,
    MaybeList (..),
    takeWhileM,
    myDiv,
    mySum,
    myMult,
  )
where

import Control.Monad.Trans.Maybe (MaybeT (..))

-- | A function for dividing numbers. Note the if-clause.
myDiv :: (Integral a) => a -> a -> Maybe a
myDiv a b = if b /= 0 && (div a b) /= 3 then Just (div a b) else Nothing

-- | A function for adding numbers.
mySum :: (Integral a) => a -> a -> Maybe a
mySum a b = Just (a + b)

-- | A function for multiplying numbers
myMult :: (Integral a) => a -> a -> Maybe a
myMult a b = Just (a * b)

-- Note: The Functor, Applicative, and Monad laws implemented and tested for these
-- structures are deeply aligned with the categorical foundations detailed in
-- "Category Theory for Programmers" by Bartosz Milewski:
-- https://ai.dmi.unibas.ch/research/reading_group/milewski-2023-01-30.pdf

data MyMaybe a = MyNothing | MyJust a deriving (Show, Eq)

instance Functor MyMaybe where
  fmap _ MyNothing = MyNothing
  fmap f (MyJust x) = MyJust (f x)

newtype MyReader a b = MyReader {unwrap :: a -> b}

instance Functor (MyReader c) where
  fmap f (MyReader g) = MyReader (f . g)

runMyReader :: MyReader a b -> a -> b
runMyReader = unwrap

newtype MaybeList a = MaybeList {getMaybeList :: [Maybe a]}
  deriving (Show, Eq)
  deriving (Functor, Applicative, Monad) via (MaybeT [])

------------------------
-- Functor Utilities ---
------------------------

-- | Monadic version of takeWhile that includes the first element that fails the predicate.
takeWhileM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
takeWhileM _ [] = return []
takeWhileM p (x : xs) = do
  ok <- p x
  if ok
    then (x :) <$> takeWhileM p xs
    else return [x] -- Return the one that failed, then stop
