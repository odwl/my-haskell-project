{-# LANGUAGE DerivingVia #-}

module Lambda.Functor
  ( MyMaybe (..),
    MyReader (..),
    runMyReader,
    MaybeList (..),
    takeWhileM,
    myDiv,
    mySum,
    myMult,
    calc,
  )
where

import Control.Monad (guard)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Functor.Const (Const (..))

-- | A function for dividing numbers. The catch is that if the result is 3, it returns Nothing.
myDiv :: (Integral a) => a -> a -> Maybe a
myDiv a b = do
  let result = div a b
  guard (b /= 0 && result /= 3)
  return result

-- | A function for adding numbers.
mySum :: (Integral a) => a -> a -> Maybe a
mySum a b = Just (a + b)

-- | A function for multiplying numbers
myMult :: (Integral a) => a -> a -> Maybe a
myMult a b = Just (a * b)

calc :: (Integral a) => a -> a -> Maybe a
calc l r = do
  x <- myDiv l r
  y <- myDiv r l
  mySum x y

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




------------------------
-- Natural Transformation ---
------------------------

-- Define a natural transformation from the Maybe functor to the list
-- functor. Prove the naturality condition for it.


alpha :: Maybe a -> [a]
alpha Nothing = []
alpha (Just x) = [x]

headNat :: [a] -> Maybe a
headNat [] = Nothing
headNat (x : _) = Just x

tailNat :: [a] -> Maybe [a]
tailNat [] = Nothing
tailNat (_ : xs) = Just xs

my_length :: [a] -> Const Int a
my_length [] = Const 0
my_length (_ : xs) = Const (1 + getConst (my_length xs))

-- f: a-> b; my_length . fmap f.....  [b] .... Const Int a that contain the length of the list 
-- fmap f. mylengtth .... Const Int b that contain the length of the list. fmap just change the useleess type.

-- Const Int String = Int 
-- fmap f (Const Int x) = Const (f x) -- I see. 

data MyFunc b a = MyFunc b

-- Proper Functor Instance
instance Functor (MyFunc b) where
  fmap f (MyFunc x) = MyFunc x

scam :: Const Int a -> Maybe a
scam (Const _) = Nothing -- Cannot invent an a.

-- | A natural transformation from the Maybe functor to the list functor.
--
-- This function converts a `Maybe a` into a list of `Maybe a`.
-- If the input is `Nothing`, it returns an empty list `[]`.
-- If the input is `Just x`, it returns a list containing a single element `[Just x]`.
--
-- The type signature `Maybe a -> [Maybe a]` demonstrates that this transformation
-- is polymorphic over `a` and preserves the `Maybe` structure within the list context.
--
-- This is a natural transformation because it satisfies the naturality condition:
-- for any function `f :: a -> b` and any `Maybe a`, the following holds:
--
-- > fmap f . nat == nat . fmap f
--
-- In other words, applying `f` before or after the natural transformation yields
-- the same result, which is a fundamental property of natural transformations.
nat :: Maybe a -> [Maybe a]
nat Nothing = []
nat (Just x) = [Just x]
