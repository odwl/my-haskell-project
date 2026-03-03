module Lambda.Functor
  ( MyMaybe (..),
    MyReader (..),
    runMyReader,
    MaybeList (..),
  )
where

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

data MaybeList a = MaybeList [Maybe a] deriving (Show, Eq) -- TODO: use Compose

instance Functor MaybeList where
  -- Use functor composition to map over the list and then the maybe
  fmap f (MaybeList ml) = MaybeList $ (fmap . fmap) f ml

instance Applicative MaybeList where
  pure x = MaybeList [Just x]

  -- Use liftA2 to compose the List and Maybe applicative logic
  MaybeList lmf <*> MaybeList lma = MaybeList $ liftA2 (<*>) lmf lma

-- instance Monad MaybeList where
-- x >>= k = ...
-- return x = ...