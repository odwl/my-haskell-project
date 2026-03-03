{-# LANGUAGE InstanceSigs #-}

module Lambda.Functor
  ( MyMaybe (..),
    MyReader (..),
    runMyReader,
    MaybeList (..),
  )
where

import Control.Monad (ap)
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
-- Hoover exercises. https://lmf.di.uminho.pt/quantum-logic-2021/LQ-Monads.pdf
-- ==========================================

data MaybeList a = MaybeList {getMaybeList :: [Maybe a]} deriving (Show, Eq)

-- TODO: use Compose
-- TODO: investigate the relationship with  MaybeT []
-- import Control.Monad.Trans.Maybe
-- newtype MaybeList a = MaybeList (MaybeT [] a)
--   deriving (Functor, Applicative, Monad)

instance Functor MaybeList where
  -- Use functor composition to map over the list and then the maybe
  fmap f (MaybeList ml) = MaybeList $ (fmap . fmap) f ml

instance Applicative MaybeList where
  pure x = MaybeList [Just x]

  -- Use ap to ensure consistency with the Monad instance
  (<*>) = ap

bindMaybe :: (a -> [Maybe b]) -> Maybe a -> [Maybe b]
bindMaybe = maybe (pure Nothing)

instance Monad MaybeList where
  (>>=) :: MaybeList a -> (a -> MaybeList b) -> MaybeList b
  (MaybeList lma) >>= f = MaybeList $ lma >>= bindMaybe (getMaybeList . f)
