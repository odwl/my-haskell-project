{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Lambda.Functor
  ( MyMaybe (..),
    MyReader (..),
    runMyReader,
    MaybeList (..),
    carEnters,
    carLeaves,
    damOpens,
    damCapacity,
  )
where

import Control.Monad.Trans.Maybe (MaybeT (..))
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

newtype MaybeList a = MaybeList {getMaybeList :: [Maybe a]}
  deriving (Show, Eq)
  deriving (Functor, Applicative, Monad) via (MaybeT [])

-- The dam opens (initial state)
damOpens :: MaybeList Int
damOpens = MaybeList [Just 0]

-- A car reaches the top of the dam.
carEnters :: Int -> MaybeList Int
carEnters nbCars =
  MaybeList $
    [Just (nbCars + 1) | nbCars < damCollapseThreshold]
      ++ [Nothing | nbCars >= damCapacity]

-- A car leaves the top of the dam. Leaving is always safe (deterministic).
carLeaves :: Int -> MaybeList Int
carLeaves nbCars
  | nbCars <= 1 = MaybeList [Just 0]
  | otherwise = MaybeList [Just (nbCars - 1)]
