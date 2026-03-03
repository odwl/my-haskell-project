{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Lambda.Functor
  ( MyMaybe (..),
    MyReader (..),
    runMyReader,
    MaybeList (..),
  )
where

import Control.Monad (ap)
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
-- ==========================================

newtype MaybeList a = MaybeList {getMaybeList :: [Maybe a]}
  deriving (Show, Eq)
  deriving (Functor, Applicative, Monad) via (MaybeT [])
