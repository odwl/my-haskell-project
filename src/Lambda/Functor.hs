{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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
    sqrtInvAddOne,
    sqrtInvAddOneKleisli,
    fishB,
    Water,
    DamState (..),
    rainStep,
    evapStep,
    oneDay,
    checkOverflow,
    capacity,
    empty,
  )
where

import Control.Monad (guard, join, (>=>))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Functor.Const (Const (..))
import Data.Maybe (fromMaybe)
import Lambda.Subdist (Subdist, certainly, makeSubdist)

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

myLength :: [a] -> Const Int a
myLength [] = Const 0
myLength (_ : xs) = Const (1 + getConst (myLength xs))

-- f: a-> b; my_length . fmap f.....  [b] .... Const Int a that contain the length of the list
-- fmap f. mylengtth .... Const Int b that contain the length of the list. fmap just change the useleess type.

-- Const Int String = Int
-- fmap f (Const Int x) = Const (f x) -- I see.

newtype MyFunc b a = MyFunc b

-- Proper Functor Instance
instance Functor (MyFunc b) where
  fmap _ (MyFunc x) = MyFunc x

scam :: Const Int a -> Maybe a
scam (Const _) = Nothing -- Cannot invent an a.

newtype Reader e a = Reader (e -> a)

instance Functor (Reader e) where
  fmap f (Reader g) = Reader (f . g)

obvious :: Reader () a -> Maybe a
obvious (Reader g) = Just (g ())

dumb :: Reader () a -> Maybe a
dumb (Reader _) = Nothing

-- Linked to Yoneda Lemma.
--
-- In other words, applying `f` before or after the natural transformation yields
-- the same result, which is a fundamental property of natural transformations.
nat :: Maybe a -> [Maybe a]
nat Nothing = []
nat (Just x) = [Just x]

------------------------
-- Identity and Composition ---
-- Challenges page 28 of pdf
-- https://ai.dmi.unibas.ch/research/reading_group/milewski-2023-01-30.pdf
------------------------

-- Implement, as best as you can, the identity function in your favorite language (or the second favorite, if your favorite language
-- happens to be Haskell).
myIdentity :: a -> a
myIdentity = id

myComp :: (a -> b) -> (b -> c) -> (a -> c)
myComp f g = g . f

-- | Write a program that tries to test that your composition function respects identity.
testIdentity :: Bool
testIdentity =
  let f = (+ 1) :: Int -> Int
      x = 10
   in (myComp myIdentity f x == f x) && (myComp f myIdentity x == f x)

-- newtype MyWriter a = MyWriter (String, a)
--   deriving (Show, Eq)
--   deriving (Functor, Applicative, Monad) via ((,) String)

-- A very simple monad to illustrate the concept of writer monad.
tell :: String -> (String, ())
tell s = (s, ())

testWriterMonad :: Bool
testWriterMonad =
  let (logg, res) = do
        let x = 3 :: Int
        let y = even x
        tell ("Start ")
        tell ("even " ++ show x ++ " ")
        tell ("not " ++ show y ++ " ")
        tell ("End")
        pure (not y)
   in logg == "Start even 3 not False End" && res

-- ==========================================
-- Kleisli
-- ==========================================

sqrtInvAddOne :: Float -> Maybe Float
sqrtInvAddOne x = do
  guard (x /= 0)
  let inv = 1 / x
  guard (inv >= 0)
  let sq = sqrt inv
  guard (sq <= 99)
  return (sq + 1)

sqrtInvAddOneKleisli :: Float -> Maybe Float
sqrtInvAddOneKleisli =
  let inv y = guard (y /= 0) >> pure (1 / y)
      sq y = guard (y >= 0) >> pure (sqrt y)
      addOne y = guard (y <= 99) >> pure (y + 1)
   in inv >=> sq >=> addOne

fishB :: (Monad m) => (a -> m b) -> (b -> m c) -> (a -> m c)
{-# ANN fishB "HLint: ignore Use =<<" #-}
fishB f g = join . fmap g . f

-- ==========================================
-- Water Simulation
-- ==========================================

type Water = Int

data DamState = OK Water | Overflowed deriving (Show, Eq, Ord)

empty :: () -> Subdist DamState
empty () = certainly (OK 0)

-- Rain adds 10L (80% chance) or 0L (20% chance)
rainStep :: DamState -> Subdist DamState
rainStep (OK current) = fromMaybe (certainly (OK current)) $ makeSubdist [(OK (current + 10), 0.8), (OK current, 0.2)]
rainStep Overflowed = certainly Overflowed

-- Evaporation removes 5L (always)
evapStep :: DamState -> Subdist DamState
evapStep (OK current) = certainly (OK (max 0 (current - 5)))
evapStep Overflowed = certainly Overflowed

oneDay :: DamState -> Subdist DamState
oneDay = rainStep >=> evapStep

capacity :: Water
capacity = 15

checkOverflow :: DamState -> Subdist DamState
checkOverflow (OK current)
  | current <= capacity = certainly (OK current)
  | otherwise = certainly Overflowed
checkOverflow Overflowed = certainly Overflowed
