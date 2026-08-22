{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Lambda.Clifford.Signature
  ( Signature(..)
  , Metric(..)
  , KnownSignature(..)
  , basisSquare
  , totalDim
  , signatureName
  -- Common standard signatures
  , Cl0_0
  , Cl0_1
  , Cl0_2
  , Cl1_0
  , Cl2_0
  , Cl3_0
  , Cl1_1
  , Cl1_3
  , Cl3_0_1
  ) where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (Nat, KnownNat, natVal)

-- | Metric signature (p, q, r) of a real vector space V:
--   * p: number of positive basis vectors (e_i² = +1)
--   * q: number of negative basis vectors (e_j² = -1)
--   * r: number of degenerate/null basis vectors (e_k² = 0, for PGA)
data Signature (p :: Nat) (q :: Nat) (r :: Nat) = Signature

-- | Evaluation of quadratic form on basis elements
data Metric = Pos | Neg | Zero
  deriving (Eq, Show, Enum)

-- | Typeclass for reifying metric signatures from type-level Nats
class (KnownNat p, KnownNat q, KnownNat r) => KnownSignature (p :: Nat) (q :: Nat) (r :: Nat) where
  sigProxy :: Proxy (Signature p q r)
  sigProxy = Proxy

instance (KnownNat p, KnownNat q, KnownNat r) => KnownSignature p q r

-- | Total dimension of generating vector space V: n = p + q + r
totalDim :: forall p q r. (KnownNat p, KnownNat q, KnownNat r) => Proxy (Signature p q r) -> Int
totalDim _ = 
  fromIntegral (natVal (Proxy :: Proxy p)) +
  fromIntegral (natVal (Proxy :: Proxy q)) +
  fromIntegral (natVal (Proxy :: Proxy r))

-- | Query the square of the k-th basis vector e_k (1-indexed: 1 <= k <= n):
--   * 1 <= k <= p         => +1
--   * p < k <= p + q      => -1
--   * p + q < k <= p+q+r  =>  0
basisSquare :: forall p q r. (KnownNat p, KnownNat q, KnownNat r) => Proxy (Signature p q r) -> Int -> Int
basisSquare _ k
  | k <= p          =  1
  | k <= p + q      = -1
  | k <= p + q + r  =  0
  | otherwise       = error $ "basisSquare: index " ++ show k ++ " out of bounds for dimension " ++ show (p + q + r)
  where
    p = fromIntegral (natVal (Proxy :: Proxy p))
    q = fromIntegral (natVal (Proxy :: Proxy q))
    r = fromIntegral (natVal (Proxy :: Proxy r))

-- | Human-readable name of standard signatures
signatureName :: forall p q r. (KnownNat p, KnownNat q, KnownNat r) => Proxy (Signature p q r) -> String
signatureName _ = "Cl(" ++ show p ++ ", " ++ show q ++ (if r > 0 then ", " ++ show r else "") ++ ")"
  where
    p = natVal (Proxy :: Proxy p)
    q = natVal (Proxy :: Proxy q)
    r = natVal (Proxy :: Proxy r)

-- | Type aliases for common geometric and physical Clifford algebras:
type Cl0_0   = Signature 0 0 0  -- ^ Real scalars ℝ
type Cl0_1   = Signature 0 1 0  -- ^ Complex numbers ℂ (i² = -1)
type Cl0_2   = Signature 0 2 0  -- ^ Quaternions ℍ (i² = j² = k² = -1)
type Cl1_0   = Signature 1 0 0  -- ^ Hyperbolic numbers (e₁² = +1)
type Cl2_0   = Signature 2 0 0  -- ^ 2D Euclidean Geometric Algebra G²
type Cl3_0   = Signature 3 0 0  -- ^ 3D Euclidean Geometric Algebra G³
type Cl1_1   = Signature 1 1 0  -- ^ (1+1)D Minkowski spacetime
type Cl1_3   = Signature 1 3 0  -- ^ 4D Dirac Spacetime Algebra (STA)
type Cl3_0_1 = Signature 3 0 1  -- ^ 3D Projective Geometric Algebra (PGA)
