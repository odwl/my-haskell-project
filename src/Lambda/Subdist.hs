module Lambda.Subdist
  ( Subdist (..),
    makeSubdist,
    simplify,
  )
where

import qualified Data.Map.Strict as Map

newtype Subdist a = Subdist {runSubdist :: [(a, Double)]}
  deriving (Show, Eq)

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

instance Functor Subdist where
  fmap f (Subdist xs) = Subdist [(f x, p) | (x, p) <- xs]

instance Applicative Subdist where
  pure x = Subdist [(x, 1.0)]
  (Subdist fs) <*> (Subdist xs) =
    Subdist [(f x, p * q) | (f, p) <- fs, (x, q) <- xs]

instance Monad Subdist where
  return = pure
  (Subdist xs) >>= f = Subdist $ do
    (x, p) <- xs
    (y, q) <- runSubdist (f x)
    return (y, p * q)

-- A helper function to expose the merged/consolidated view if needed,
-- since we can't restrict Functor/Monad to (Ord a) in Haskell
simplify :: (Ord a) => Subdist a -> Subdist a
simplify = Subdist . consolidate . runSubdist
