{-# LANGUAGE InstanceSigs #-}

module Lambda.HoverDam
  ( CarCount (..),
    carEnters,
    carLeaves,
    damOpens,
    damCapacity,
    damCollapseThreshold,
  )
where

import Data.Maybe (fromJust)
import Lambda.Subdist (Subdist, makeSubdist)
import Prelude

-- ==========================================
-- Inspired by: https://lmf.di.uminho.pt/quantum-logic-2021/LQ-Monads.pdf
-- Exercice: Hover Dam
-- ==========================================

newtype CarCount = CarCount Int
  deriving (Show, Eq, Ord)

-- A car reaches the top of the dam.
damCapacity :: CarCount
damCapacity = CarCount 3

-- The absolute physical limit before guaranteed collapse.
damCollapseThreshold :: CarCount
damCollapseThreshold = CarCount 4

-- The dam opens (initial state)
damOpens :: Subdist CarCount
damOpens = fromJust $ makeSubdist [(CarCount 0, 1.0)]

-- A car reaches the top of the dam.
carEnters :: CarCount -> Subdist CarCount
carEnters (CarCount nbCars)
  | CarCount nbCars < damCapacity = fromJust $ makeSubdist [(CarCount (nbCars + 1), 1.0)] -- 100% safe
  | CarCount nbCars == damCapacity = fromJust $ makeSubdist [(CarCount (nbCars + 1), 0.5)] -- 50% safe, 50% crash (mass lost)
  | otherwise = fromJust $ makeSubdist [] -- 100% crash

-- A car leaves the top of the dam. Leaving is always safe (deterministic).
carLeaves :: CarCount -> Subdist CarCount
carLeaves (CarCount nbCars)
  | nbCars <= 1 = fromJust $ makeSubdist [(CarCount 0, 1.0)]
  | otherwise = fromJust $ makeSubdist [(CarCount (nbCars - 1), 1.0)]
