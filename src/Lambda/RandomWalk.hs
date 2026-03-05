{-# LANGUAGE LambdaCase #-}

module Lambda.RandomWalk
  ( Action (..),
    RandomWalk (..),
    applyAction,
    genBoundedActions,
    analyzePath,
  )
where

import Control.Monad (foldM)
import Data.Map (Map)
import qualified Data.Map as Map
import Test.QuickCheck (Gen, elements)

-- ==========================================
-- Random Walk Types
-- ==========================================

data Action = Inc | Dec deriving (Show, Eq)

-- | A random walk consisting of a starting state and a generator of actions.
data RandomWalk = RandomWalk Int (Gen [Action])

applyAction :: Action -> Int -> Int
applyAction Inc n = n + 1
applyAction Dec n = n - 1

-- | Generates a 'RandomWalk' that, when actions are applied sequentially,
-- keeps the value within [minB, maxB].
--
-- * 'minB': The minimum allowed state boundary.
-- * 'maxB': The maximum allowed state boundary.
-- * 'start': The initial state to start the walk from.
-- * 'numSteps': The number of steps (actions) to generate.
genBoundedActions :: Int -> Int -> Int -> Int -> RandomWalk
genBoundedActions minB maxB start numSteps = RandomWalk start $ do
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

-- | Analyzes a 'RandomWalk' from its starting state to count the number of visits
-- to each state.
--
-- * 'walk': The 'RandomWalk' to analyze.
--
-- Returns a 'Map' wrapped in 'Gen' where each key is a visited state and the value
-- is the number of times that state was reached (including the starting state).
analyzePath :: RandomWalk -> Gen (Map Int Int)
analyzePath (RandomWalk start genActions) = do
  actions <- genActions
  let path = scanl (flip applyAction) start actions
  return $ Map.fromListWith (+) [(state, 1) | state <- path]
