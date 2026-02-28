{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}

module Lambda.State where

import Control.Arrow ((&&&))
import Data.Bifunctor (first, second)

newtype State s a = State
  { runState :: s -> (a, s)
  }

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State g) = State (first f . g)

instance Applicative (State s) where
  pure :: a -> State s a
  pure x = State ((,) x)
  (<*>) :: State s (a -> b) -> State s a -> State s b
  (State pg) <*> (State px) = State (uncurry first . second px . pg)

instance Monad (State s) where
  (State g) >>= f = State (uncurry runState . first f . g)

get :: State s s
get = State (id &&& id)

put :: s -> State s ()
put = State . const . (,) ()

modify :: (s -> s) -> State s ()
modify f = State ((,) () . f)

evalState :: State s a -> s -> a
evalState m s = fst (runState m s)

execState :: State s a -> s -> s
execState m s = snd (runState m s)

data Expr a = Var a | Add (Expr a) (Expr a) deriving (Show, Functor, Eq)

instance Applicative Expr where
  pure :: a -> Expr a
  pure = Var

  (<*>) :: Expr (a -> b) -> Expr a -> Expr b
  Var f <*> mx = f <$> mx
  Add f1 f2 <*> mx = Add (f1 <*> mx) (f2 <*> mx)

instance Monad Expr where
  (>>=) :: Expr a -> (a -> Expr b) -> Expr b
  Var x >>= f = f x
  Add e1 e2 >>= f = Add (e1 >>= f) (e2 >>= f)
