{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Lambda.Clifford.Versor
  ( -- * Type-Level Parity Classification
    Parity(..)
  , MulParity
  -- * Core Versor Type (Constructor is Opaque)
  , Versor
  , unVersor
  , Rotor
  , Pinor
  -- * Infix Composition Operator
  , (⊗)
  -- * Smart Constructors (Correct-by-Construction)
  , fromVector
  , fromVectorPair
  , fromVectorProduct
  , reflectionFromUnitVector
  , rotorFromPlaneAngle
  , rotorFromTwoVectors
  -- * Dynamic Validation Smart Constructors
  , isVersor
  , isEvenVersor
  , isOddVersor
  , mkVersor
  , mkRotor
  , mkPinor
  -- * Parity Coercion & Refinement
  , toAnyVersor
  , toEvenVersor
  , toOddVersor
  -- * Inversion & Involutions
  , invertRotor
  , invertVersor
  , gradeInvolution
  -- * Geometric Actions (Sandwich Transformations)
  , applyRotor
  , applyPinor
  , applyVersor
  -- * Quaternion / 3D Even Multivector Factorization
  , factorEvenCl300
  , factorEvenCl030
  -- * Unsafe Escape Hatch (for Internal Extensions)
  , unsafeMkVersor
  ) where

import qualified Data.Map.Strict as Map
import GHC.TypeLits (Nat)

import Lambda.Clifford.Signature
import Lambda.Clifford.Universal

--------------------------------------------------------------------------------
-- 1. Type-Level Parity Classification
--------------------------------------------------------------------------------

-- | Parity classification of a Clifford multivector / versor:
--   * 'Even': Pure even multivector (elements of the even subalgebra Cl⁺, e.g. Rotors / Spinors)
--   * 'Odd':  Pure odd multivector (elements of Cl⁻, e.g. Reflectors / Pinors)
--   * 'Any':  Unspecified / dynamically determined parity
data Parity = Even | Odd | Any
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Closed Type Family calculating the parity of a geometric product at compile-time:
--   * Even ⊗ Even = Even   (Rotations compose to a rotation)
--   * Odd  ⊗ Odd  = Even   (Two reflections compose to a rotation!)
--   * Even ⊗ Odd  = Odd    (Rotation and reflection compose to a reflection)
--   * Odd  ⊗ Even = Odd    (Reflection and rotation compose to a reflection)
type family MulParity (p1 :: Parity) (p2 :: Parity) :: Parity where
  MulParity 'Even 'Even = 'Even
  MulParity 'Odd  'Odd  = 'Even
  MulParity 'Even 'Odd  = 'Odd
  MulParity 'Odd  'Even = 'Odd
  MulParity _     _     = 'Any

--------------------------------------------------------------------------------
-- 2. Core Versor Data Type
--------------------------------------------------------------------------------

-- | A Versor in Cl(p, q, r): a multivector factorable into a geometric product
--   of non-null 1-vectors (V = v₁ · v₂ · ... · vₖ).
--
-- The data constructor 'VersorUnsafe' is NOT exported to prevent invalid
-- multivectors (such as uninvertible nilpotents or mixed-parity non-versors).
newtype Versor (k :: Parity) (p :: Nat) (q :: Nat) (r :: Nat) a = VersorUnsafe
  { unVersor :: Clifford p q r a }
  deriving (Eq)

-- | Read-only Show instance formatting as Versor(...) or Rotor(...)
instance Show a => Show (Versor k p q r a) where
  show (VersorUnsafe v) = "Versor(" ++ show v ++ ")"

-- | A normalized Even Versor (Rotor): elements of Spin(p, q) / SO(p, q)
type Rotor p q r a = Versor 'Even p q r a

-- | An Odd Versor (Reflector / Pinor): elements of Pin(p, q) / O(p, q)
type Pinor p q r a = Versor 'Odd p q r a

-- | Unsafe internal constructor exported only for expert extensions
unsafeMkVersor :: Clifford p q r a -> Versor k p q r a
unsafeMkVersor = VersorUnsafe

--------------------------------------------------------------------------------
-- 3. Algebraic Group Instances & Composition Operator
--------------------------------------------------------------------------------

-- | Rotors form a lawful Monoid under geometric product (closed under Even ⊗ Even = Even)
instance (KnownSignature p q r, ExactScalar a) => Semigroup (Versor 'Even p q r a) where
  VersorUnsafe r1 <> VersorUnsafe r2 = VersorUnsafe (r1 * r2)

instance (KnownSignature p q r, ExactScalar a) => Monoid (Versor 'Even p q r a) where
  mempty = VersorUnsafe unitClifford

-- | General versors of 'Any parity form a Monoid under geometric product
instance (KnownSignature p q r, ExactScalar a) => Semigroup (Versor 'Any p q r a) where
  VersorUnsafe v1 <> VersorUnsafe v2 = VersorUnsafe (v1 * v2)

instance (KnownSignature p q r, ExactScalar a) => Monoid (Versor 'Any p q r a) where
  mempty = VersorUnsafe unitClifford

infixl 7 ⊗
-- | Type-safe versor geometric composition respecting compile-time parity:
--
-- >>> let r1 = ... :: Rotor 2 0 0 Double
-- >>> let r2 = ... :: Rotor 2 0 0 Double
-- >>> r1 ⊗ r2  -- Returns Rotor 2 0 0 Double ('Even)
--
-- >>> let p1 = ... :: Pinor 2 0 0 Double
-- >>> let p2 = ... :: Pinor 2 0 0 Double
-- >>> p1 ⊗ p2  -- Returns Rotor 2 0 0 Double ('Even, two reflections form a rotation!)
(⊗) :: (KnownSignature p q r, ExactScalar a)
    => Versor k1 p q r a
    -> Versor k2 p q r a
    -> Versor (MulParity k1 k2) p q r a
VersorUnsafe v1 ⊗ VersorUnsafe v2 = VersorUnsafe (v1 * v2)

--------------------------------------------------------------------------------
-- 4. Smart Constructors (Correct-by-Construction)
--------------------------------------------------------------------------------

-- | Construct a Pinor (Odd Versor / Reflector) from a single 1-vector:
--   Verifies that the input is a pure Grade-1 vector with non-zero norm (v² /= 0).
fromVector :: forall p q r a. (KnownSignature p q r, ExactScalar a)
           => Clifford p q r a
           -> Maybe (Pinor p q r a)
fromVector v
  | isPureNonZeroVector v = Just (VersorUnsafe v)
  | otherwise             = Nothing
  where
    isPureNonZeroVector vec =
      grade 1 vec == vec && scalarProd vec (reverseCl vec) /= 0

-- | Construct a Rotor (Even Versor / Rotation) from a pair of 1-vectors (u · v):
fromVectorPair :: forall p q r a. (KnownSignature p q r, ExactScalar a)
               => Clifford p q r a
               -> Clifford p q r a
               -> Maybe (Rotor p q r a)
fromVectorPair u v
  | isPureNonZeroVector u && isPureNonZeroVector v =
      Just (VersorUnsafe (u * v))
  | otherwise = Nothing
  where
    isPureNonZeroVector vec =
      grade 1 vec == vec && scalarProd vec (reverseCl vec) /= 0

-- | Construct a general Versor from an arbitrary sequence of non-null 1-vectors:
--   V = v₁ · v₂ · ... · vₖ
fromVectorProduct :: forall p q r a. (KnownSignature p q r, ExactScalar a)
                  => [Clifford p q r a]
                  -> Maybe (Versor 'Any p q r a)
fromVectorProduct [] = Just (VersorUnsafe unitClifford)
fromVectorProduct vecs
  | all isPureNonZeroVector vecs = Just (VersorUnsafe (product vecs))
  | otherwise                    = Nothing
  where
    isPureNonZeroVector vec =
      grade 1 vec == vec && scalarProd vec (reverseCl vec) /= 0

-- | Construct a normalized Pinor reflection across the plane orthogonal to unit vector n:
reflectionFromUnitVector :: forall p q r a. (KnownSignature p q r, ExactScalar a)
                         => Clifford p q r a
                         -> Maybe (Pinor p q r a)
reflectionFromUnitVector n
  | grade 1 n == n && abs (scalarProd n (reverseCl n)) == 1 =
      Just (VersorUnsafe n)
  | otherwise = Nothing

-- | Construct a Rotor from an angle θ and a unit planar bivector blade B (B² = -1):
--   R = exp(-θ B / 2) = cos(θ / 2) - B · sin(θ / 2)
rotorFromPlaneAngle :: forall p q r a. (KnownSignature p q r, ExactScalar a, Floating a)
                    => Blade
                    -> a
                    -> Maybe (Rotor p q r a)
rotorFromPlaneAngle b theta
  | bladeGrade b == 2 =
      let half = theta / 2
          c = cos half
          s = sin half
          r = scalar c - blade b s
      in Just (VersorUnsafe r)
  | otherwise = Nothing

-- | Construct the Rotor rotating unit vector u into unit vector v:
--   R = (1 + v · u) / ||1 + v · u||
rotorFromTwoVectors :: forall p q r a. (KnownSignature p q r, ExactScalar a, Floating a)
                    => Clifford p q r a
                    -> Clifford p q r a
                    -> Maybe (Rotor p q r a)
rotorFromTwoVectors u v
  | grade 1 u == u && grade 1 v == v =
      let m = scalar 1 + (v * u)
          normSq = scalarProd m (reverseCl m)
      in if normSq == 0
           then Nothing -- Vectors are antiparallel (180° degeneracy) or null
           else Just (VersorUnsafe (m * scalar (1 / sqrt normSq)))
  | otherwise = Nothing

--------------------------------------------------------------------------------
-- 5. Mathematical Validation & Outermorphism Test
--------------------------------------------------------------------------------

-- | Grade Involution (Main Automorphism α): α(⟨A⟩ₖ) = (-1)ᵏ · ⟨A⟩ₖ
--   Flips the sign of odd-grade components while leaving even-grade invariant.
gradeInvolution :: forall p q r a. (KnownSignature p q r, ExactScalar a)
                => Clifford p q r a
                -> Clifford p q r a
gradeInvolution (Clifford m) =
  Clifford (Map.mapWithKey (\b nz -> if odd (bladeGrade b) then negate <$> nz else nz) m)

-- | Test whether an arbitrary multivector is a valid geometric versor:
--   1. Homogeneous in parity (pure even or pure odd).
--   2. Non-zero norm (⟨V ~V⟩₀ /= 0, invertible).
--   3. Outermorphism invariant: maps all 1-vectors to pure 1-vectors under sandwiching!
isVersor :: forall p q r a. (KnownSignature p q r, Fractional a, ExactScalar a, Ord a)
         => Clifford p q r a
         -> Bool
isVersor mv =
  hasPureParity mv
  && isInvertible mv
  && preservesVectorSpace mv
  where
    hasPureParity m =
      let evenPart = sum [ grade k m | k <- [0, 2 .. totalDim @p @q @r] ]
          oddPart  = sum [ grade k m | k <- [1, 3 .. totalDim @p @q @r] ]
      in isNegligible (m - evenPart) || isNegligible (m - oddPart)

    isInvertible m =
      let normSq = scalarProd m (reverseCl m)
      in normSq * normSq > 1e-24

    preservesVectorSpace m =
      all (checkBasis m) [1 .. totalDim @p @q @r]

    checkBasis m k =
      let ek = basisIndex @p @q @r k
          transformed = rawSandwich m ek
          leak = transformed - grade 1 transformed
      in isNegligible leak

    isNegligible (Clifford m) =
      all (\(_, NonZero c) -> c * c <= 1e-14) (Map.toList m)

    rawSandwich v x =
      let revV = reverseCl v
          normSq = scalarProd v revV
          vInv = revV * scalar (1 / normSq)
      in gradeInvolution v * x * vInv

-- | Test whether a multivector is a valid pure Even Versor (Rotor candidate)
isEvenVersor :: forall p q r a. (KnownSignature p q r, Fractional a, ExactScalar a, Ord a)
             => Clifford p q r a
             -> Bool
isEvenVersor mv =
  let evenPart = sum [ grade k mv | k <- [0, 2 .. totalDim @p @q @r] ]
  in mv == evenPart && isVersor mv

-- | Test whether a multivector is a valid pure Odd Versor (Pinor candidate)
isOddVersor :: forall p q r a. (KnownSignature p q r, Fractional a, ExactScalar a, Ord a)
            => Clifford p q r a
            -> Bool
isOddVersor mv =
  let oddPart = sum [ grade k mv | k <- [1, 3 .. totalDim @p @q @r] ]
  in mv == oddPart && isVersor mv

-- | Dynamic Smart Constructor for Versors of 'Any parity
mkVersor :: forall p q r a. (KnownSignature p q r, Fractional a, ExactScalar a, Ord a)
         => Clifford p q r a
         -> Maybe (Versor 'Any p q r a)
mkVersor mv
  | isVersor mv = Just (VersorUnsafe mv)
  | otherwise   = Nothing

-- | Dynamic Smart Constructor for pure Even Versors (Rotors)
mkRotor :: forall p q r a. (KnownSignature p q r, Fractional a, ExactScalar a, Ord a)
        => Clifford p q r a
        -> Maybe (Rotor p q r a)
mkRotor mv
  | isEvenVersor mv = Just (VersorUnsafe mv)
  | otherwise       = Nothing

-- | Dynamic Smart Constructor for pure Odd Versors (Pinors)
mkPinor :: forall p q r a. (KnownSignature p q r, Fractional a, ExactScalar a, Ord a)
        => Clifford p q r a
        -> Maybe (Pinor p q r a)
mkPinor mv
  | isOddVersor mv = Just (VersorUnsafe mv)
  | otherwise      = Nothing

--------------------------------------------------------------------------------
-- 6. Parity Refinement & Coercion
--------------------------------------------------------------------------------

-- | Forget type-level parity to 'Any
toAnyVersor :: Versor k p q r a -> Versor 'Any p q r a
toAnyVersor (VersorUnsafe v) = VersorUnsafe v

-- | Refine an 'Any versor to 'Even (Rotor) if its odd components are zero
toEvenVersor :: forall p q r a. (KnownSignature p q r, ExactScalar a)
             => Versor 'Any p q r a
             -> Maybe (Rotor p q r a)
toEvenVersor (VersorUnsafe v) =
  let oddPart = sum [ grade k v | k <- [1, 3 .. totalDim @p @q @r] ]
  in if oddPart == zeroClifford
       then Just (VersorUnsafe v)
       else Nothing

-- | Refine an 'Any versor to 'Odd (Pinor) if its even components are zero
toOddVersor :: forall p q r a. (KnownSignature p q r, ExactScalar a)
            => Versor 'Any p q r a
            -> Maybe (Pinor p q r a)
toOddVersor (VersorUnsafe v) =
  let evenPart = sum [ grade k v | k <- [0, 2 .. totalDim @p @q @r] ]
  in if evenPart == zeroClifford
       then Just (VersorUnsafe v)
       else Nothing

--------------------------------------------------------------------------------
-- 7. Inversion & Geometric Actions (Sandwich Product)
--------------------------------------------------------------------------------

-- | Invert a normalized Rotor: R⁻¹ = ~R (since R · ~R = 1)
invertRotor :: ExactScalar a => Rotor p q r a -> Rotor p q r a
invertRotor (VersorUnsafe r) = VersorUnsafe (reverseCl r)

-- | Invert a general Versor of arbitrary norm: V⁻¹ = ~V / ⟨V · ~V⟩₀
invertVersor :: forall k p q r a. (KnownSignature p q r, Fractional a, ExactScalar a)
             => Versor k p q r a
             -> Versor k p q r a
invertVersor (VersorUnsafe v) =
  let revV = reverseCl v
      normSq = scalarProd v revV
  in VersorUnsafe (revV * scalar (1 / normSq))

-- | Rotor Action on any Multivector X:
--   X ↦ R · X · ~R
applyRotor :: (KnownSignature p q r, ExactScalar a)
           => Rotor p q r a
           -> Clifford p q r a
           -> Clifford p q r a
applyRotor (VersorUnsafe r) x = r * x * reverseCl r

-- | Pinor (Reflection) Action on any Multivector X:
--   X ↦ - n · X · n⁻¹
applyPinor :: (KnownSignature p q r, Fractional a, ExactScalar a)
           => Pinor p q r a
           -> Clifford p q r a
           -> Clifford p q r a
applyPinor p x =
  let (VersorUnsafe v) = p
      (VersorUnsafe vInv) = invertVersor p
  in negateClifford v * x * vInv

-- | General Versor Action on any Multivector X:
--   X ↦ α(V) · X · V⁻¹
applyVersor :: forall k p q r a. (KnownSignature p q r, Fractional a, ExactScalar a)
            => Versor k p q r a
            -> Clifford p q r a
            -> Clifford p q r a
applyVersor v x =
  let (VersorUnsafe vCl) = v
      (VersorUnsafe vInv) = invertVersor v
  in gradeInvolution vCl * x * vInv

--------------------------------------------------------------------------------
-- 8. 3D Even Multivector (Quaternion) Vector Factorization
--------------------------------------------------------------------------------

-- | Constructively factor any non-zero even multivector (quaternion) in Cl(3, 0, 0)
--   into a geometric product of two 1-vectors (u, v) such that @u * v == q@.
factorEvenCl300 :: forall a. (ExactScalar a, Floating a, Ord a)
                => Clifford 3 0 0 a
                -> Maybe (Clifford 3 0 0 a, Clifford 3 0 0 a)
factorEvenCl300 q
  | not (isEvenCl q) = Nothing
  | normSq < 1e-12   = Nothing
  | bNormSq < 1e-12  =
      let s = scalarCoeff q
          e1 = basisIndex @3 @0 @0 1
      in if s >= 0
           then let sq = sqrt s
                    u  = scalar sq * e1
                in Just (u, u)
           else let sq = sqrt (negate s)
                    u  = scalar sq * e1
                in Just (u, negate u)
  | otherwise =
      let iVol  = basisIndex @3 @0 @0 1 * basisIndex @3 @0 @0 2 * basisIndex @3 @0 @0 3
          n     = negate (bPart * iVol)
          n1    = bladeCoeff 1 n
          n2    = bladeCoeff 2 n
          n3    = bladeCoeff 4 n
          e1    = basisIndex @3 @0 @0 1
          e2    = basisIndex @3 @0 @0 2
          e3    = basisIndex @3 @0 @0 3
          uRaw
            | abs n1 <= abs n2 && abs n1 <= abs n3 =
                scalar n3 * e2 - scalar n2 * e3
            | abs n2 <= abs n3 =
                scalar (negate n3) * e1 + scalar n1 * e3
            | otherwise =
                scalar n2 * e1 - scalar n1 * e2
          uLen = sqrt (scalarCoeff (uRaw * reverseCl uRaw))
          u    = scalar (1 / uLen) * uRaw
          v    = grade 1 (u * q)
      in Just (u, v)
  where
    normSq  = scalarProd q (reverseCl q)
    bPart   = grade 2 q
    bNormSq = scalarProd bPart (reverseCl bPart)
    isEvenCl m = m == sum [ grade k m | k <- [0, 2 .. totalDim @3 @0 @0] ]

-- | Constructively factor any non-zero even multivector in Cl(0, 3, 0)
--   into a geometric product of two 1-vectors (u, v) such that @u * v == q@.
factorEvenCl030 :: forall a. (ExactScalar a, Floating a, Ord a)
                => Clifford 0 3 0 a
                -> Maybe (Clifford 0 3 0 a, Clifford 0 3 0 a)
factorEvenCl030 q
  | not (isEvenCl q) = Nothing
  | normSq < 1e-12   = Nothing
  | bNormSq < 1e-12  =
      let s = scalarCoeff q
          e1 = basisIndex @0 @3 @0 1
          u  = e1
          v  = scalar (negate s) * e1
      in Just (u, v)
  | otherwise =
      let iVol  = basisIndex @0 @3 @0 1 * basisIndex @0 @3 @0 2 * basisIndex @0 @3 @0 3
          n     = negate (bPart * iVol)
          n1    = bladeCoeff 1 n
          n2    = bladeCoeff 2 n
          n3    = bladeCoeff 4 n
          e1    = basisIndex @0 @3 @0 1
          e2    = basisIndex @0 @3 @0 2
          e3    = basisIndex @0 @3 @0 3
          uRaw
            | abs n1 <= abs n2 && abs n1 <= abs n3 =
                scalar n3 * e2 - scalar n2 * e3
            | abs n2 <= abs n3 =
                scalar (negate n3) * e1 + scalar n1 * e3
            | otherwise =
                scalar n2 * e1 - scalar n1 * e2
          uLen = sqrt (negate (scalarCoeff (uRaw * uRaw)))
          u    = scalar (1 / uLen) * uRaw
          v    = negate (grade 1 (u * q))
      in Just (u, v)
  where
    normSq  = scalarCoeff (q * reverseCl q)
    bPart   = grade 2 q
    bNormSq = scalarCoeff (negate (bPart * bPart))
    isEvenCl m = m == sum [ grade k m | k <- [0, 2 .. totalDim @0 @3 @0] ]
