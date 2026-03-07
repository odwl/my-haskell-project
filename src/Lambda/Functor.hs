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
    process,
    process2,
    testProcess,
    testProcess2,
  )
where

import Control.Monad (guard, (>=>))
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
  fmap _ (MyFunc x) = MyFunc x

scam :: Const Int a -> Maybe a
scam (Const _) = Nothing -- Cannot invent an a.

newtype Reader e a = Reader (e -> a)
instance Functor (Reader e) where
  fmap f (Reader g) = Reader (f . g)

obvious:: Reader () a -> Maybe a
obvious (Reader g) = Just (g ())

dumb:: Reader () a -> Maybe a
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
my_identity :: a -> a
my_identity = id

my_comp :: (a -> b) -> (b -> c) -> (a -> c)
my_comp f g = g . f

-- | Write a program that tries to test that your composition function respects identity.
testIdentity :: Bool
testIdentity =
  let f = (+ 1) :: Int -> Int
      x = 10
   in (my_comp my_identity f x == f x) && (my_comp f my_identity x == f x)

-- memoize :: Ord a => (a -> b) -> (a -> b)
-- memoize f = mem_f
--   where 
--     cache = Map.empty
--     mem_f x = 
--       case Map.lookup x cache of
--         Just y -> y
--         Nothing -> 
--           let y = f x
--           in Map.insert x y cache


-- f: Bool -> Bool 
-- f True = True
-- f False = False 

  




testZob2 :: Bool
testZob2 =
  let (logg, res) = do
        let x = 3 :: Int
        let y = even x
        tell ("Start ") >> tell ("even " ++ show x ++ " ")
        tell ("not " ++ show y ++ " ")
        tell ("End") >> pure (not y)
   in logg == "Start even 3 not False End" && res == True

-- (>=>) :: (a -> (String, b)) -> (b -> (String, c)) -> (a -> (String, c))
-- f >=> g = \x -> f x >>= g

process :: Float -> Maybe Float
process x = do
  guard (x /= 0)
  let inv = 1 / x
  guard (inv >= 0)
  let sq = sqrt inv
  guard (sq <= 99)
  return (sq + 1)

process2 :: Float -> Maybe Float
process2 = 
  let inv y = guard (y /= 0) >> pure (1 / y)
      sq y = guard (y >= 0) >> pure (sqrt y)
      addOne y = guard (y <= 99) >> pure (y+1)
  in inv >=> sq >=> addOne

testProcess :: Bool
testProcess = 
  process 4.0 == Just 1.5 && 
  process 0.0 == Nothing && 
  process 0.0001 == Nothing && 
  process (-1.0) == Nothing

testProcess2 :: Bool
testProcess2 = 
  process2 4.0 == Just 1.5 && 
  process2 0.0 == Nothing && 
  process2 0.0001 == Nothing && 
  process2 (-1.0) == Nothing

