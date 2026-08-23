{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Lambda.Clifford.Universal
  ( -- * Core Universal Multivector Type 
    Clifford(..)
  , Blade
  , bladeGrade
  , basisBladeName
  -- * Non-Zero Scalar Restriction
  , NonZero(..)
  , mkNonZero
  , oneNZ
  -- * Constructors
  , scalar
  , basis
  , basisIndex
  , blade
  , bladeNZ
  , fromBladeList
  , toBladeList
  -- * Geometric Operations
  , grade
  , grades
  , reverseCl
  , geometricProduct
  , wedge
  , scalarProd
  , cliffordScalar
  , leftContract
  , fatDot
  , dotHestenes
  -- * Universal Property Homomorphism
  , universalFold
  -- * Infix Operators
  , (∧)
  , (·)
  , (∗)
  , (⨼)
  , (●)
  , (•)
  ) where

import Control.Arrow ((>>>))
import Data.Bits (Bits(..), popCount, countTrailingZeros)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy(..))
import Data.List (intercalate)
import GHC.TypeLits (Nat, KnownNat, natVal, type (<=), type (+))

import Lambda.Clifford.Signature

-- | A Blade is a bitmask where bit k represents the presence of basis vector e_(k+1).
--   * 0 (0b00) = 1 (Scalar, Grade 0)
--   * 1 (0b01) = e₁ (Vector, Grade 1)
--   * 2 (0b10) = e₂ (Vector, Grade 1)
--   * 3 (0b11) = e₁₂ = e₁e₂ (Bivector, Grade 2)
type Blade = Word

-- | Grade of a blade is the number of 1-bits (popCount)
bladeGrade :: Blade -> Int 
bladeGrade = popCount

-- | Convert digit char to Unicode subscript: '1' -> '₁', '2' -> '₂'
toSubscript :: Char -> Char
toSubscript '0' = '₀'; toSubscript '1' = '₁'; toSubscript '2' = '₂'
toSubscript '3' = '₃'; toSubscript '4' = '₄'; toSubscript '5' = '₅'
toSubscript '6' = '₆'; toSubscript '7' = '₇'; toSubscript '8' = '₈'
toSubscript '9' = '₉'; toSubscript c   = c

-- | Extract list of active basis vector indices (1-indexed)
bitIndices :: Blade -> [Int]
bitIndices 0 = []
bitIndices m = ((countTrailingZeros m) + 1) : bitIndices (m .&. (m - 1))

-- | Human-readable string representation of a basis blade: 0 -> "1", 3 -> "e₁₂", 7 -> "e₁₂₃"
basisBladeName :: Blade -> String
basisBladeName 0 = "1"
basisBladeName b = "e" ++ map toSubscript (concatMap show (bitIndices b))

-- | A scalar coefficient guaranteed to NEVER be zero.
-- The constructor 'NonZeroUnsafe' is NOT exported to external users.
newtype NonZero a = NonZeroUnsafe { unNonZero :: a }
  deriving (Eq, Ord, Show, Read)
-- | Smart Constructor: Returns 'Nothing' if the value is zero.
mkNonZero :: (Num a, Eq a) => a -> Maybe (NonZero a)
mkNonZero 0 = Nothing
mkNonZero x = Just (NonZeroUnsafe x)
-- | Idiomatic Pattern Synonym (Read-Only View Pattern):
-- Allows clean pattern matching `case nz of NonZero x -> ...`
-- while preventing raw construction of `NonZero 0`!
pattern NonZero :: a -> NonZero a
pattern NonZero x <- NonZeroUnsafe x

-- | The multiplicative unit constant 1 (guaranteed non-zero)
oneNZ :: Num a => NonZero a
oneNZ = NonZeroUnsafe 1

-- | Universal Multivector parameterized by signature Cl(p, q, r) and scalar type a
newtype Clifford (p :: Nat) (q :: Nat) (r :: Nat) a = Clifford
  { unClifford :: Map Blade (NonZero a) }
  deriving (Eq)

-- | Construct a 1-vector basis element e_k from a dynamic runtime index (1 <= k <= n)
basisIndex :: forall p q r a. (KnownSignature p q r, Num a, Eq a) => Int -> Clifford p q r a
basisIndex k
  | k >= 1 && k <= totalDim (Proxy :: Proxy (Signature p q r)) =
      bladeNZ (bit (k - 1)) oneNZ
  | otherwise = error $ "basisIndex: index " ++ show k ++ " out of range"
 
-- | Construct a single blade term: c * e_(indices)
blade :: (Num a, Eq a) => Blade -> a -> Clifford p q r a
blade b c = case mkNonZero c of 
  Nothing -> Clifford Map.empty
  Just nz -> bladeNZ b nz

bladeNZ :: Blade -> NonZero a -> Clifford p q r a
bladeNZ b nz = Clifford (Map.singleton b nz)

-- | Construct a pure Grade-0 scalar multivector
scalar :: forall p q r a. (Num a, Eq a) => a -> Clifford p q r a
scalar = blade 0

-- | Construct a 1-vector basis element e_k with compile-time index verification: 1 <= k <= (p + q + r)
basis :: forall (k :: Nat) p q r a.
         ( KnownNat k
         , 1 <= k
         , k <= (p + q + r)
         , Num a
         ) => Clifford p q r a
basis = bladeNZ (bit (kVal - 1)) oneNZ
  where
    kVal = fromIntegral (natVal (Proxy @k))

-- | Build a multivector from a list of (Blade, coefficient) pairs
fromBladeList :: (Num a, Eq a) => [(Blade, a)] -> Clifford p q r a
fromBladeList = Map.fromListWith (+) >>> Map.mapMaybe mkNonZero >>> Clifford

-- | Convert multivector to a sorted list of (Blade, coefficient) pairs
toBladeList :: Clifford p q r a -> [(Blade, a)]
toBladeList (Clifford m) = [(b, unNonZero nz) | (b, nz) <- Map.toAscList m]

-- | Show instance formatting multivectors as linear combinations of basis blades:
--
-- >>> show (scalar 0 :: Clifford 2 0 0 Int)
-- "0"
-- >>> show (scalar 5 :: Clifford 2 0 0 Int)
-- "5"
-- >>> show (scalar 5 + basis @1 * 3 - basis @2 * 4 + (basis @1 * basis @2) * 7 :: Clifford 2 0 0 Int)
-- "5 + 3·e₁ + -4·e₂ + 7·e₁₂"
instance Show a => Show (Clifford p q r a) where
  show = toBladeList >>> \case
    []    -> "0"
    pairs -> intercalate " + " (map formatTerm pairs)
    where
      formatTerm (b, c) = show c ++ bladeSuffix b
      bladeSuffix 0 = ""
      bladeSuffix b = "·" ++ basisBladeName b

-- | Multiply two basis blades under metric signature Cl(p, q, r):
--   1. Combined blade = b1 `xor` b2
--   2. Sign parity = (-1)^(number of bit swaps needed to sort indices)
--   3. Metric scaling = product of basisSquare(k+1) for all common bits in (b1 .&. b2)
multiplyBlades :: forall p q r a. (KnownSignature p q r, Num a, Eq a)
               => Proxy (Signature p q r) -> Blade -> Blade -> (Blade, a)
multiplyBlades proxy b1 b2
  | metricFactor == 0 = (0, 0)
  | otherwise         = (b1 `xor` b2, fromIntegral (swapSign * metricFactor))
  where
    -- Count inversions/swaps to sort interleaved basis vectors
    swapCount = countSwaps b2 0
    countSwaps 0 acc = acc
    countSwaps m acc =
      let j = countTrailingZeros m
          higherBitsInB1 = popCount (b1 `shiftR` (j + 1))
      in countSwaps (m .&. (m - 1)) (acc + higherBitsInB1)

    swapSign = if even swapCount then 1 else -1

    -- Multiply quadratic form values for overlapping basis vectors (e_k²)
    metricFactor = computeMetric (b1 .&. b2) 1
    computeMetric 0 acc = acc
    computeMetric m acc =
      let k = countTrailingZeros m
          sq = basisSquare proxy (k + 1)
      in if sq == 0 then 0 else computeMetric (m .&. (m - 1)) (acc * sq)

-- | The Universal Geometric Product (*) on arbitrary dimensions
geometricProduct :: forall p q r a. (KnownSignature p q r, Num a, Eq a)
                 => Clifford p q r a -> Clifford p q r a -> Clifford p q r a
geometricProduct (Clifford m1) (Clifford m2) =
  Clifford $ Map.mapMaybe mkNonZero $ Map.fromListWith (+)
    [ (resBlade, unNonZero c1 * unNonZero c2 * signVal)
    | (b1, c1) <- Map.toList m1
    , (b2, c2) <- Map.toList m2
    , let (resBlade, signVal) = multiplyBlades (Proxy :: Proxy (Signature p q r)) b1 b2
    , signVal /= 0
    ]

-- | Grade projection: extract all blade terms of homogeneous grade k
grade :: (Num a, Eq a) => Int -> Clifford p q r a -> Clifford p q r a
grade k (Clifford m) = Clifford (Map.filterWithKey (\b _ -> bladeGrade b == k) m)

-- | Decompose into a list of homogeneous grade components [Grade 0, Grade 1, ...]
grades :: forall p q r a. (KnownSignature p q r, Num a, Eq a) => Clifford p q r a -> [Clifford p q r a]
grades mv = [ grade k mv | k <- [0 .. totalDim (Proxy :: Proxy (Signature p q r))] ]

-- | Clifford Reversion (~A): reverses the order of basis vectors
--   For a homogeneous grade-k blade: ~⟨A⟩_k = (-1)^(k(k-1)/2) * ⟨A⟩_k
reverseCl :: (Num a, Eq a) => Clifford p q r a -> Clifford p q r a
reverseCl (Clifford m) =
  Clifford $ Map.mapWithKey (\b (NonZero c) -> if even (gradeSignExp (bladeGrade b)) then NonZeroUnsafe c else NonZeroUnsafe (negate c)) m
  where
    gradeSignExp k = (k * (k - 1)) `div` 2

-- | Exterior / Wedge Product (∧): Grade-summing outer product
--   ⟨A⟩_r ∧ ⟨B⟩_s = ⟨A * B⟩_(r+s)
wedge :: forall p q r a. (KnownSignature p q r, Num a, Eq a)
      => Clifford p q r a -> Clifford p q r a -> Clifford p q r a
wedge (Clifford m1) (Clifford m2) =
  Clifford $ Map.mapMaybe mkNonZero $ Map.fromListWith (+)
    [ (resBlade, unNonZero c1 * unNonZero c2 * signVal)
    | (b1, c1) <- Map.toList m1
    , (b2, c2) <- Map.toList m2
    -- Grade condition: bladeGrade(resBlade) == bladeGrade(b1) + bladeGrade(b2)
    -- This happens iff b1 and b2 share NO common basis vectors: (b1 .&. b2) == 0
    , (b1 .&. b2) == 0
    , let (resBlade, signVal) = multiplyBlades (Proxy :: Proxy (Signature p q r)) b1 b2
    , signVal /= 0
    ]

-- | Scalar Product (⟨A * B⟩₀): extracts Grade-0 scalar component of geometric product
scalarProd :: (KnownSignature p q r, Num a, Eq a) => Clifford p q r a -> Clifford p q r a -> a
scalarProd a b = maybe 0 unNonZero (Map.lookup 0 (unClifford (geometricProduct a b)))

-- | Clifford Scalar Product (⟨A * ~B⟩₀): strictly positive-definite Hilbert inner product
cliffordScalar :: (KnownSignature p q r, Num a, Eq a) => Clifford p q r a -> Clifford p q r a -> a
cliffordScalar a b = scalarProd a (reverseCl b)

-- | Left Contraction (⨼ / ⌋):
--   ⟨A⟩_r ⨼ ⟨B⟩_s = ⟨A * B⟩_(s - r)  when r <= s
--   ⟨A⟩_r ⨼ ⟨B⟩_s = 0                when r > s
leftContract :: forall p q r a. (KnownSignature p q r, Num a, Eq a)
             => Clifford p q r a -> Clifford p q r a -> Clifford p q r a
leftContract (Clifford m1) (Clifford m2) =
  Clifford $ Map.mapMaybe mkNonZero $ Map.fromListWith (+)
    [ (resBlade, unNonZero c1 * unNonZero c2 * signVal)
    | (b1, c1) <- Map.toList m1
    , (b2, c2) <- Map.toList m2
    , let r = bladeGrade b1
          s = bladeGrade b2
    , r <= s
    , let (resBlade, signVal) = multiplyBlades (Proxy :: Proxy (Signature p q r)) b1 b2
    , bladeGrade resBlade == (s - r)
    , signVal /= 0
    ]

-- | Fat Dot Product (●): ⟨A⟩_r ● ⟨B⟩_s = ⟨A * B⟩_|r - s| for all grades r, s >= 0
fatDot :: forall p q r a. (KnownSignature p q r, Num a, Eq a)
       => Clifford p q r a -> Clifford p q r a -> Clifford p q r a
fatDot (Clifford m1) (Clifford m2) =
  Clifford $ Map.mapMaybe mkNonZero $ Map.fromListWith (+)
    [ (resBlade, unNonZero c1 * unNonZero c2 * signVal)
    | (b1, c1) <- Map.toList m1
    , (b2, c2) <- Map.toList m2
    , let diff = abs (bladeGrade b1 - bladeGrade b2)
          (resBlade, signVal) = multiplyBlades (Proxy :: Proxy (Signature p q r)) b1 b2
    , bladeGrade resBlade == diff
    , signVal /= 0
    ]

-- | Hestenes Inner Product (•): ⟨A⟩_r • ⟨B⟩_s = ⟨A * B⟩_|r - s| (for r, s > 0, scalars drop to 0)
dotHestenes :: forall p q r a. (KnownSignature p q r, Num a, Eq a)
            => Clifford p q r a -> Clifford p q r a -> Clifford p q r a
dotHestenes (Clifford m1) (Clifford m2) =
  Clifford $ Map.mapMaybe mkNonZero $ Map.fromListWith (+)
    [ (resBlade, unNonZero c1 * unNonZero c2 * signVal)
    | (b1, c1) <- Map.toList m1
    , (b2, c2) <- Map.toList m2
    , let r = bladeGrade b1
          s = bladeGrade b2
    , r > 0 && s > 0
    , let diff = abs (r - s)
          (resBlade, signVal) = multiplyBlades (Proxy :: Proxy (Signature p q r)) b1 b2
    , bladeGrade resBlade == diff
    , signVal /= 0
    ]

-- | The Universal Property Homomorphism (Universal Fold):
--   Given an evaluation function for 1-vectors in an algebra `evalBasis :: Int -> alg`,
--   uniquely extends to evaluate arbitrary multivectors!
universalFold :: forall p q r a alg. (KnownSignature p q r, Num alg)
              => (Int -> alg) -> (a -> alg) -> Clifford p q r a -> alg
universalFold evalBasis evalScalar (Clifford m) =
  sum [ evalScalar (unNonZero c) * evalBlade b | (b, c) <- Map.toList m ]
  where
    evalBlade 0 = 1
    evalBlade b = product [ evalBasis (i + 1) | i <- setBitIndices b ]
    
    setBitIndices 0 = []
    setBitIndices msk =
      let i = countTrailingZeros msk
      in i : setBitIndices (msk .&. (msk - 1))

-- | Num instance making Clifford an Associative Unital Ring
instance (KnownSignature p q r, Num a, Eq a) => Num (Clifford p q r a) where
  (Clifford m1) + (Clifford m2) = Clifford $
    Map.mergeWithKey
      (\_ (NonZero x) (NonZero y) -> mkNonZero (x + y))
      id
      id
      m1
      m2
  (*) = geometricProduct
  negate (Clifford m) = Clifford (Map.map (\(NonZero x) -> NonZeroUnsafe (negate x)) m)
  abs _ = scalar 1 -- symbolic ring abs
  signum _ = scalar (fromInteger 1)
  fromInteger n = scalar (fromInteger n)

-- | Infix Operators
infixl 7 ∧
(∧) :: (KnownSignature p q r, Num a, Eq a) => Clifford p q r a -> Clifford p q r a -> Clifford p q r a
(∧) = wedge

infixl 7 ·
(·) :: (KnownSignature p q r, Num a, Eq a) => Clifford p q r a -> Clifford p q r a -> Clifford p q r a
(·) a b = scalar (scalarProd a b)

infixl 7 ∗
(∗) :: (KnownSignature p q r, Num a, Eq a) => Clifford p q r a -> Clifford p q r a -> Clifford p q r a
(∗) a b = scalar (cliffordScalar a b)

infixl 7 ⨼
(⨼) :: (KnownSignature p q r, Num a, Eq a) => Clifford p q r a -> Clifford p q r a -> Clifford p q r a
(⨼) = leftContract

infixl 7 ●
(●) :: (KnownSignature p q r, Num a, Eq a) => Clifford p q r a -> Clifford p q r a -> Clifford p q r a
(●) = fatDot

infixl 7 •
(•) :: (KnownSignature p q r, Num a, Eq a) => Clifford p q r a -> Clifford p q r a -> Clifford p q r a
(•) = dotHestenes
