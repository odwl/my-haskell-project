{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Lambda.Subdist
  ( Subdist,
    runSubdist,
    runSubdistRaw,
    makeSubdist,
    certainly,
    impossible,
    weighted,
    simplify,
  )
where

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Enumerator (Enumerator, explicit, fromList)
import qualified Data.Map.Strict as Map

-- | Subdist represents a probabilistic distribution that can have a total weight <= 1.0.
-- We wrap the monad-bayes Enumerator, which handles discrete probabilistic choices.
newtype Subdist a = Subdist {getEnumerator :: Enumerator a}
  deriving (Functor, Applicative, Monad, MonadDistribution, MonadFactor, MonadMeasure) via Enumerator

instance (Show a) => Show (Subdist a) where
  show = show . runSubdistRaw

instance (Ord a) => Eq (Subdist a) where
  (Subdist m) == (Subdist n) = eqDist (consolidate (explicit m)) (consolidate (explicit n))
    where
      eqDist [] [] = True
      eqDist ((x1, p1) : xs) ((x2, p2) : ys) = x1 == x2 && abs (p1 - p2) < 1e-9 && eqDist xs ys
      eqDist _ _ = False

instance (Ord a) => Ord (Subdist a) where
  compare (Subdist m) (Subdist n) = compare (consolidate (explicit m)) (consolidate (explicit n))

-- | Executes the distribution and returns the consolidated (summed and sorted) results.
-- Merges identical outcomes and ignores those with near-zero probability.
runSubdist :: (Ord a) => Subdist a -> [(a, Double)]
runSubdist = consolidate . runSubdistRaw

-- | Returns the raw distribution trails (unconsolidated).
-- Useful for debugging the individual probabilistic paths.
runSubdistRaw :: Subdist a -> [(a, Double)]
runSubdistRaw = explicit . getEnumerator

-- | Represents a deterministic outcome with probability 1.0.
certainly :: a -> Subdist a
certainly = pure

-- | Represents an impossible outcome (empty distribution).
impossible :: Subdist a
impossible = Subdist (fromList [])

-- | Constructs a distribution for a single outcome with a given probability.
-- Clamps probability to [0, 1].
weighted :: a -> Double -> Subdist a
weighted x p
  | p <= 0.0 = impossible
  | p >= 1.0 = certainly x
  | otherwise = Subdist (fromList [(x, Exp (log p))])

-- | Smart constructor that enforces sub-probability invariants:
-- 1. All weights are non-negative.
-- 2. The total sum of weights is <= 1.0 (allowing for tiny floating drift).
makeSubdist :: [(a, Double)] -> Maybe (Subdist a)
makeSubdist xs
  | all (\(_, p) -> p >= 0.0) xs && sum (map snd xs) <= 1.0 + 1e-6 =
      Just $ Subdist $ fromList [(x, Exp (log p)) | (x, p) <- xs]
  | otherwise = Nothing

-- Helper to merge duplicate elements by adding their probabilities
consolidate :: (Ord a) => [(a, Double)] -> [(a, Double)]
consolidate = Map.toList . Map.fromListWith (+) . filter ((> 1e-12) . snd)

-- | Simplifies the distribution by merging identical outcomes.
-- Note: Enumerator is already normalized internaly for some operations,
-- but 'explicit' preserves the structure.
simplify :: (Ord a) => Subdist a -> Subdist a
simplify = Subdist . fromList . map (\(x, p) -> (x, Exp (log p))) . consolidate . runSubdistRaw
