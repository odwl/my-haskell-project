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

-- A car reaches the top of the dam. There can only be three cars
-- on the dam at the same time
damCapacity :: Int
damCapacity = 3

carEnters :: Int -> Maybe Int
carEnters x = if x < damCapacity then Just (x + 1) else Nothing

-- A car leaves the top of the dam.
carLeaves :: Int -> Maybe Int
carLeaves x = if x > 1 then Just (x - 1) else Just 0

-- The dam opens (initial state)
damOpens :: Maybe Int
damOpens = Just 0

newtype MaybeList a = MaybeList {getMaybeList :: [Maybe a]}
  deriving (Show, Eq)
  deriving (Functor, Applicative, Monad) via (MaybeT [])
