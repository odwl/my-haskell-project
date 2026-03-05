{-# LANGUAGE LambdaCase #-}

module Lambda.RandomWalk
  ( Action (..),
    RandomWalk (..),
    genInfiniteActions,
    applyAction,
    applyReflectingBounds,
    applyAbsorbingBounds,
    analyzePath,
  )
where

import Data.List (mapAccumL)
import Data.Map (Map)
import qualified Data.Map as Map
import Test.QuickCheck (Gen, elements, infiniteListOf)

-- ==========================================
-- Random Walk Types
-- ==========================================

data Action = Inc | Dec deriving (Show, Eq)

genInfiniteActions :: Gen [Action]
genInfiniteActions = infiniteListOf (elements [Inc, Dec])

-- | A random walk consisting of a starting state and a generator of actions.
data RandomWalk = RandomWalk Int (Gen [Action])

applyAction :: Action -> Int -> Int
applyAction Inc n = n + 1
applyAction Dec n = n - 1

-- | Takes an infinite stream of actions and "reflects" them off the boundaries
applyReflectingBounds :: Int -> Int -> Int -> [Action] -> [Action]
applyReflectingBounds minB maxB start actions =
  snd $ mapAccumL step start actions
  where
    step curr action =
      let canInc = curr < maxB
          canDec = curr > minB

          -- Modify the action if it hits a wall
          boundedAction = case (canInc, canDec, action) of
            (False, True, Inc) -> Dec -- Hit the ceiling, bounce down
            (True, False, Dec) -> Inc -- Hit the floor, bounce up
            _ -> action -- Safe to proceed
       in (applyAction boundedAction curr, boundedAction)

-- | Takes a stream of actions and stops if it exceeds the boundaries
applyAbsorbingBounds :: Int -> Int -> Int -> [Action] -> [Action]
applyAbsorbingBounds minB maxB start = go start
  where
    go _ [] = []
    go curr (a : as) =
      let next = applyAction a curr
       in if next < minB || next > maxB
            then [a] -- Include the violating action then stop
            else a : go next as

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
