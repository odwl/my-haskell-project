{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lambda.FunctorTestUtils where

import Control.Monad (foldM)
import Control.Monad.Reader
import Lambda.Functor
import Test.QuickCheck
import Test.QuickCheck.Checkers

-- ==========================================
-- Random Walk Actions
-- ==========================================

data Action = Inc | Dec deriving (Show, Eq)

applyAction :: Action -> Int -> Int
applyAction Inc n = n + 1
applyAction Dec n = n - 1

-- | Generates a list of actions (Inc or Dec) that, when applied sequentially
-- starting from 'start', keep the value within [minB, maxB].
genBoundedActions :: Int -> Int -> Int -> Int -> Gen [Action]
genBoundedActions minB maxB start numSteps = do
  (_, actions) <- foldM next (start, []) [1 .. numSteps]
  return (reverse actions)
  where
    next (curr, acc) _ = do
      let canInc = curr < maxB
          canDec = curr > minB

      action <- case (canInc, canDec) of
        (True, True) -> elements [Inc, Dec]
        (True, False) -> return Inc
        (False, True) -> return Dec
        (False, False) -> return Inc -- Should not happen if minB < maxB
      return (applyAction action curr, action : acc)

-- ==========================================
-- Hover Dam Domains
-- ==========================================

-- Generates a list of movement functions (carEnters or carLeaves) that stay within [0, damCapacity]
genSafeMovesStartingAt :: Int -> Gen [Int -> MaybeList Int]
genSafeMovesStartingAt start = do
  numSteps <- choose (0, 50 :: Int)
  actions <- genBoundedActions 0 damCapacity start numSteps
  return $ map (\case Inc -> carEnters; Dec -> carLeaves) actions

genSafeMoves :: Gen [Int -> MaybeList Int]
genSafeMoves = genSafeMovesStartingAt 0

-- ==========================================
-- Arbitrary and EqProp Instances
-- ==========================================

instance (Arbitrary a) => Arbitrary (MyMaybe a) where
  arbitrary =
    frequency
      [ (1, pure MyNothing),
        (3, fmap MyJust arbitrary)
      ]

instance (Eq a) => EqProp (MyMaybe a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (MaybeList a) where
  arbitrary = fmap MaybeList (scale (\n -> min n 5) arbitrary)

instance (Eq a) => EqProp (MaybeList a) where
  (=-=) :: (Eq a) => MaybeList a -> MaybeList a -> Property
  (=-=) = eq

-- ==========================================
-- Testing Helpers
-- ==========================================

-- A custom operator for "Extensional Equality" of Readers
eqReader :: (Eq a, Show a) => Reader r a -> Reader r a -> r -> Property
eqReader r1 r2 x = runReader r1 x === runReader r2 x

eqMyReader :: (Eq b, Show b) => MyReader a b -> MyReader a b -> a -> Property
eqMyReader m1 m2 x = runMyReader m1 x === runMyReader m2 x
