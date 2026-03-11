{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Lambda.Functor
  ( MyProxy (..),
    MyIdentity (..),
    MyConst (..),
    MyEither (..),
    MyMaybe2 (..),
    MyMaybe (..),
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
    emptyDam,
    Op (..),
    MyLog (..),
    Logger,
    writerComputation,
  )
where

import Control.Monad (guard, join, (>=>))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Writer (Writer, writer)
import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Const (Const (..))
import Data.Maybe (fromMaybe)
import Lambda.Subdist (Subdist, certainly, makeSubdist)
import Prelude

---------------------------
-- Functor from Scratch ---
---------------------------

data MyProxy a = MyProxy deriving (Eq, Show)

instance Functor MyProxy where
  fmap _ MyProxy = MyProxy

newtype MyIdentity a = Id a deriving (Eq, Show)

instance Functor MyIdentity where
  fmap f (Id x) = Id (f x)

newtype MyConst a b = MyConst a deriving (Eq, Show)

instance Functor (MyConst a) where
  fmap _ (MyConst cons) = MyConst cons

data MyEither a b = MyLeft a | MyRight b deriving (Eq, Show)

instance Functor (MyEither a) where
  fmap _ (MyLeft a) = MyLeft a
  fmap f (MyRight b) = MyRight (f b)

instance Bifunctor MyEither where
  first f (MyLeft a) = MyLeft (f a)
  first _ (MyRight b) = MyRight b

  second _ (MyLeft a) = MyLeft a
  second g (MyRight b) = MyRight (g b)

newtype MyMaybe2 a = MyMaybe2 (MyEither (MyProxy a) (MyIdentity a)) deriving (Eq, Show)

instance Functor MyMaybe2 where
  fmap f (MyMaybe2 inner) = MyMaybe2 (bimap (fmap f) (fmap f) inner)

data MyMaybe a = MyNothing | MyJust a deriving (Show, Eq)

instance Functor MyMaybe where
  fmap _ MyNothing = MyNothing
  fmap f (MyJust x) = MyJust (f x)

---------------------------
-- A little exercise on the Maybe Monad ---
---------------------------

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

newtype MyReader a b = MyReader {unwrap :: a -> b}

instance Functor (MyReader c) where
  fmap f (MyReader g) = MyReader (f . g)

runMyReader :: MyReader a b -> a -> b
runMyReader = unwrap

newtype MaybeList a = MaybeList {getMaybeList :: [Maybe a]}
  deriving (Show, Eq)
  deriving (Functor, Applicative, Monad) via (MaybeT [])

------------------------
-- MyList ---
------------------------

data MyList a = Nil | Cons a (MyList a)

instance Functor MyList where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap (const (f x)) xs)

instance Applicative MyList where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  Cons f fs <*> xs = fmap f xs `append` (fs <*> xs)
    where
      append Nil ys = ys
      append (Cons z zs) ys = Cons z (append zs ys)

------------------------
-- Functor Bifunctor ---
------------------------
myBimap :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
myBimap f g (x, y) = (f x, g y)

myFirst :: (a -> b) -> (a, c) -> (b, c)
myFirst f (x, y) = (f x, y)

mySecond :: (a -> b) -> (c, a) -> (c, b)
mySecond = fmap

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

------------------------
-- Writer Monad ---
------------------------

-- | Represents a specific operation executed during computation.
data Op = EVEN | NOT
  deriving (Show, Eq)

-- | The MyLog newtype tracks the sequence of operations
newtype MyLog = MyLog [(Op, Bool)]
  deriving (Show, Eq)
  deriving (Semigroup, Monoid) via [(Op, Bool)]

type Logger = Writer MyLog

logOp :: Op -> Bool -> Logger Bool
logOp op b = writer (b, MyLog [(op, b)])

applyEven :: Int -> Logger Bool
applyEven = logOp EVEN . even

applyNot :: Bool -> Logger Bool
applyNot = logOp NOT . not

writerComputation :: Int -> Logger Bool
writerComputation = applyEven >=> applyNot

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

emptyDam :: Subdist DamState
emptyDam = certainly (OK 0)

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
checkOverflow state = pure $ case state of
  OK current | current > capacity -> Overflowed
  _ -> state
