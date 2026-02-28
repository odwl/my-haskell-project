{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}

module Lambda.State where

import Control.Arrow ((&&&))
import Control.Monad (ap)
import Data.Bifunctor (first, second)
import Data.Word

newtype State s a = State {runState :: s -> (a, s)}

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

type Random a = State Int a

fresh :: Random Word8
fresh = State $ \i ->
  let a = 6364136223846793005 :: Integer
      c = 1442695040888963407 :: Integer
      m = 2 ^ (64 :: Integer)
      next = (a * toInteger i + c) `mod` m
   in (fromIntegral (toInteger next `mod` 256), fromIntegral next)

runPRNG :: Random a -> Int -> a
runPRNG m s = evalState m s

-- | Example: Turn any expression into one with random variables.
-- This uses the Fact that Expr is Traversable!
randomizeExpr :: Expr a -> Random (Expr Word8)
randomizeExpr = traverse (const fresh)

{-
-- This is what 'traverse' is doing for you "under the hood":
randomizeExprManual :: Expr a -> Random (Expr Int)
randomizeExprManual (Var _) = do
  n <- fresh       -- Pull a random number/update state
  return (Var n)
randomizeExprManual (Add e1 e2) = do
  e1' <- randomizeExprManual e1  -- Process left side (passing seed along)
  e2' <- randomizeExprManual e2  -- Process right side (using new seed)
  return (Add e1' e2')           -- Rebuild the tree
-}

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
