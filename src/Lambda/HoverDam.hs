module Lambda.HoverDam
  ( CarCount (..),
    DamStrategy,
    carEnters,
    carLeaves,
    damOpens,
    damCapacity,
    damCollapseThreshold,
    -- Strategies
    strategyStep,
    strategyTwoStep,
    strategyLinear,
  )
where

import Lambda.Subdist (Subdist, certainly, weighted)
import Prelude

-- ==========================================
-- Inspired by: https://lmf.di.uminho.pt/quantum-logic-2021/LQ-Monads.pdf
-- Exercice: Hover Dam
-- ==========================================

newtype CarCount = CarCount Int
  deriving (Show, Eq, Ord)

-- | A strategy determines the probability a car can enter given the new count.
type DamStrategy = CarCount -> Double

-- A car reaches the top of the dam.
damCapacity :: CarCount
damCapacity = CarCount 3

-- The absolute physical limit before guaranteed collapse.
damCollapseThreshold :: CarCount
damCollapseThreshold = CarCount 4

-- The dam opens (initial state)
damOpens :: Subdist CarCount
damOpens = certainly (CarCount 0)

-- ==========================================
-- Strategies
-- ==========================================

-- | Step: 100% up to capacity, 0% beyond (no threshold window).
strategyStep :: DamStrategy
strategyStep count
  | count <= damCapacity = 1.0
  | otherwise = 0.0

-- | Two-Step: 100% up to capacity, 50% at threshold, 0% beyond.
strategyTwoStep :: DamStrategy
strategyTwoStep count
  | count <= damCapacity = 1.0
  | count == damCollapseThreshold = 0.5
  | otherwise = 0.0

-- | Linear: probability decreases linearly, p(n) = max(0, 1 - n/10).
strategyLinear :: DamStrategy
strategyLinear (CarCount n) = max 0.0 (min 1.0 (1.0 - fromIntegral n / 10.0))

-- ==========================================
-- Dam Operations
-- ==========================================

-- | A car attempts to enter the dam using a given strategy.
carEnters :: DamStrategy -> CarCount -> Subdist CarCount
carEnters strategy nbCars =
  let nextCount = case nbCars of CarCount n -> CarCount (n + 1)
   in weighted nextCount $ strategy nextCount

-- A car leaves the top of the dam. Leaving is always safe (deterministic).
carLeaves :: CarCount -> Subdist CarCount
carLeaves (CarCount nbCars)
  | nbCars <= 1 = certainly (CarCount 0)
  | otherwise = certainly (CarCount (nbCars - 1))
