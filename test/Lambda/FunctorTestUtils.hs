{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lambda.FunctorTestUtils
  ( Action (..),
    WalkResult (..),
    applyAction,
    genBoundedActions,
    genSafeMovesStartingAt,
    genSafeMoves,
    genHoverDamWalk,
    eqReader,
    eqMyReader,
  )
where

import Control.Monad (foldM)
import Control.Monad.Reader
import Lambda.Functor
import Lambda.Subdist (Subdist)
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
genSafeMovesStartingAt :: Int -> Gen [Int -> Subdist Int]
genSafeMovesStartingAt start = do
  numSteps <- choose (0, 50 :: Int)
  actions <- genBoundedActions 0 damCapacity start numSteps
  return $ map (\case Inc -> carEnters; Dec -> carLeaves) actions

genSafeMoves :: Gen [Int -> Subdist Int]
genSafeMoves = genSafeMovesStartingAt 0

-- ==========================================
-- Enhanced Walk Tracking
-- ==========================================

data WalkResult = WalkResult
  { wrPath :: [Int], -- Sequence of states
    wrRiskCount :: Int, -- Number of times an 'Inc' was performed at 'damCapacity'
    wrCollapsed :: Bool -- True if a state > 'damCollapseThreshold' was reached
  }
  deriving (Show, Eq)

-- | Generates a random walk for the hover dam simulation.
-- It returns a list of functions (carEnters or carLeaves),
-- and tracks how many times 'carEnters' was called when the dam was at capacity.
genHoverDamWalk :: Int -> Int -> Gen (WalkResult, [Int -> Subdist Int])
genHoverDamWalk start numSteps = do
  (res, moves) <- foldM next (WalkResult [start] 0 False, []) [1 .. numSteps]
  return (res {wrPath = reverse (wrPath res)}, reverse moves)
  where
    next (res, moves) _ = do
      let curr = head (wrPath res)
      if wrCollapsed res
        then return (res, moves)
        else do
          action <- elements [Inc, Dec]
          let -- Match physical transitions exactly
              nextState = case action of
                Inc -> curr + 1
                Dec -> if curr <= 1 then 0 else curr - 1
              isRisk = action == Inc && curr == damCapacity
              isCollapse = action == Inc && curr >= damCollapseThreshold
              move = case action of
                Inc -> carEnters
                Dec -> carLeaves
          return
            ( res
                { wrPath = nextState : wrPath res,
                  wrRiskCount = if isRisk then wrRiskCount res + 1 else wrRiskCount res,
                  wrCollapsed = isCollapse
                },
              move : moves
            )

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

-- ==========================================
-- Testing Helpers
-- ==========================================

-- A custom operator for "Extensional Equality" of Readers
eqReader :: (Eq a, Show a) => Reader r a -> Reader r a -> r -> Property
eqReader r1 r2 x = runReader r1 x === runReader r2 x

eqMyReader :: (Eq b, Show b) => MyReader a b -> MyReader a b -> a -> Property
eqMyReader m1 m2 x = runMyReader m1 x === runMyReader m2 x
