{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}

module Lambda.State (module Lambda.State, module Control.Monad.State) where

import Control.Monad (ap)
import Control.Monad.State (State, state)
import Data.Word

type Random a = State Integer a

lcgMultiplier :: Integer
lcgMultiplier = 6364136223846793005

lcgIncrement :: Integer
lcgIncrement = 1442695040888963407

lcgModulus :: Integer
lcgModulus = 2 ^ (64 :: Integer)

fresh :: Random Word8
fresh = state $ \i ->
  let next = (lcgMultiplier * i + lcgIncrement) `mod` lcgModulus
   in (fromIntegral next, next)

randomizeExpr :: Expr a -> Random (Expr Word8)
randomizeExpr = traverse (const fresh)

data Expr a = Var a | Add (Expr a) (Expr a) deriving (Show, Functor, Eq, Foldable, Traversable)

instance Applicative Expr where
  pure :: a -> Expr a
  pure = Var

  (<*>) :: Expr (a -> b) -> Expr a -> Expr b
  (<*>) = ap

instance Monad Expr where
  (>>=) :: Expr a -> (a -> Expr b) -> Expr b
  Var x >>= f = f x
  Add e1 e2 >>= f = Add (e1 >>= f) (e2 >>= f)

replace :: (Eq a) => [(a, b)] -> Expr a -> Expr (Maybe b)
replace l = fmap (`lookup` l)

convert :: Expr (Maybe a) -> Maybe (Expr a)
convert = traverse id
