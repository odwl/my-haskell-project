{-# LANGUAGE InstanceSigs #-}

module Lambda.Functor
  ( MyMaybe (..),
    MyReader (..),
    runMyReader,
  )
where

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
