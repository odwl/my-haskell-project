module Lambda.RandomWalk
  ( Action (..),
    RandomWalk (..),
    genInfiniteActions,
    applyAction,
    applyReflectingBounds,
    applyAbsorbingBounds,
    pathSnapshots,
  )
where

import Control.Monad.State (State, evalState, get, gets, modify, put)
import Data.Map (Map)
import qualified Data.Map as Map
import Lambda.Functor (takeWhileM)
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
  evalState (traverse step actions) start
  where
    step :: Action -> State Int Action
    step action = do
      let reflect currentState = case action of
            Inc | currentState >= maxB -> Dec
            Dec | currentState <= minB -> Inc
            _ -> action

      newAction <- gets reflect
      modify (applyAction newAction)
      return newAction

-- | Takes a stream of actions and stops if it exceeds the boundaries
applyAbsorbingBounds :: Int -> Int -> Int -> [Action] -> [Action]
applyAbsorbingBounds minB maxB start actions =
  evalState (takeWhileM check actions) start
  where
    check :: Action -> State Int Bool
    check action = do
      currentState <- get
      let next = applyAction action currentState
      if next >= minB && next <= maxB
        then put next >> return True
        else return False

-- | Returns an infinite stream (list) of 'Map' snapshots, where each map
-- represents the visit counts up to that point in the walk.
--
-- This is safe to use on infinite walks because it builds the maps lazily.
pathSnapshots :: RandomWalk -> Gen [Map Int Int]
pathSnapshots (RandomWalk start genActions) = do
  actions <- genActions
  let path = scanl (flip applyAction) start actions
  return $ scanl (\m pos -> Map.insertWith (+) pos 1 m) (Map.singleton start 1) (tail path)
