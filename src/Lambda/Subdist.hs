{-# LANGUAGE DerivingVia #-}

module Lambda.Subdist
  ( Subdist, -- Export type only
    runSubdist, -- Export accessor
    makeSubdist, -- Export smart constructor
    simplify,
  )
where

import Control.Monad.Trans.Writer (WriterT (..))
import qualified Data.Map.Strict as Map
import Data.Monoid (Product (..))

newtype Subdist a = Subdist {runSubdist :: [(a, Double)]}
  deriving (Show, Eq)
  deriving (Functor, Applicative, Monad) via WriterT (Product Double) []

-- | Smart constructor that enforces sub-probability invariants:
-- 1. All weights are non-negative.
-- 2. The total sum of weights is <= 1.0 (allowing for tiny floating drift).
makeSubdist :: [(a, Double)] -> Maybe (Subdist a)
makeSubdist xs
  | all (\(_, p) -> p >= 0.0) xs && sum (map snd xs) <= 1.0 + 1e-6 =
      Just (Subdist xs)
  | otherwise = Nothing

-- Helper to merge duplicate elements by adding their probabilities
consolidate :: (Ord a) => [(a, Double)] -> [(a, Double)]
consolidate = Map.toList . Map.fromListWith (+) . filter ((> 0) . snd)

-- Note: To satisfy laws using EqProp, we cannot just derive Eq,
-- but the instances below don't technically require Ord. However, to
-- implement a *true* probability monad where equivalent paths sum up,
-- `Subdist` would usually require `Ord` on its inner types, which breaks standard Monad.
-- For standard Monad laws without `Ord` constraints, we must NOT consolidate
-- during `>>=`, but instead rely on a custom `EqProp` instance that treats
-- unconsolidated lists as equivalent if their grouped sums match.

-- A helper function to expose the merged/consolidated view if needed,
-- since we can't restrict Functor/Monad to (Ord a) in Haskell
simplify :: (Ord a) => Subdist a -> Subdist a
simplify = Subdist . consolidate . runSubdist
