{-# LANGUAGE InstanceSigs #-}

module Lambda.Functor
  ( MyMaybe (..),
    MyReader (..),
    runMyReader,
    carEnters,
    carLeaves,
    damOpens,
    damCapacity,
    damCollapseThreshold,
  )
where

import Data.Maybe (fromJust)
import Lambda.Subdist (Subdist, makeSubdist)
import Prelude

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

-- ==========================================
-- Inspired by: https://lmf.di.uminho.pt/quantum-logic-2021/LQ-Monads.pdf
-- Exercice: Hover Dam
-- ==========================================

-- A car reaches the top of the dam.
damCapacity :: Int
damCapacity = 3

-- The absolute physical limit before guaranteed collapse.
damCollapseThreshold :: Int
damCollapseThreshold = 4

-- The dam opens (initial state)
damOpens :: Subdist Int
damOpens = fromJust $ makeSubdist [(0, 1.0)]

-- A car reaches the top of the dam.
carEnters :: Int -> Subdist Int
carEnters nbCars
  | nbCars < damCapacity = fromJust $ makeSubdist [(nbCars + 1, 1.0)] -- 100% safe
  | nbCars == damCapacity = fromJust $ makeSubdist [(nbCars + 1, 0.5)] -- 50% safe, 50% crash (mass lost)
  | otherwise = fromJust $ makeSubdist [] -- 100% crash

-- A car leaves the top of the dam. Leaving is always safe (deterministic).
carLeaves :: Int -> Subdist Int
carLeaves nbCars
  | nbCars <= 1 = fromJust $ makeSubdist [(0, 1.0)]
  | otherwise = fromJust $ makeSubdist [(nbCars - 1, 1.0)]
