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
  , bladeGrade2
  , bladeToIndices
  , indicesToBlade
  , basisBladeName
  , isEvenSwaps
  , multBlades
  -- * Non-Zero Scalar Restriction
  , NonZero(..)
  , pattern NonZero
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
bladeGrade2 :: Blade -> Int 
bladeGrade2 = popCount

-- | Convert digit char to Unicode subscript: '1' -> '₁', '2' -> '₂'
toSubscript :: Char -> Char
toSubscript '0' = '₀'; toSubscript '1' = '₁'; toSubscript '2' = '₂'
toSubscript '3' = '₃'; toSubscript '4' = '₄'; toSubscript '5' = '₅'
toSubscript '6' = '₆'; toSubscript '7' = '₇'; toSubscript '8' = '₈'
toSubscript '9' = '₉'; toSubscript c   = c

-- | Extract list of active basis vector indices (1-indexed) in strictly ascending order.
--
-- Examples:
-- >>> bladeToIndices 0   -- Scalar unit 1
-- []
-- >>> bladeToIndices 3   -- 0b0011 (e₁₂)
-- [1,2]
-- >>> bladeToIndices 13  -- 0b1101 (e₁₃₄)
-- [1,3,4]
bladeToIndices :: Blade -> [Int]
bladeToIndices 0 = []
bladeToIndices m = ((countTrailingZeros m) + 1) : bladeToIndices (m .&. (m - 1))

-- | Convert strictly sorted [Int] to bitmask.
--
-- Examples:
-- >>> indicesToBlade []         -- Scalar unit 1
-- 0
-- >>> indicesToBlade [1, 2]     -- e₁₂
-- 3
-- >>> indicesToBlade [1, 3, 4]  -- e₁₃₄
-- 13
indicesToBlade :: [Int] -> Blade
indicesToBlade = foldr ff 0
  where ff x acc = setBit acc (x - 1)

-- | Human-readable string representation of a basis blade: 0 -> "1", 3 -> "e₁₂", 7 -> "e₁₂₃"
basisBladeName :: Blade -> String
basisBladeName 0 = "1"
basisBladeName b = "e" ++ map toSubscript (concatMap show (bladeToIndices b))

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
-- | Check whether the number of basis vector swaps required to sort b1 and b2 is even.
--   Returns 'True' for positive sign (+1), 'False' for negative sign (-1).
--
-- Examples:
-- >>> isEvenSwaps 1 2   -- e₁ * e₂ (0 swaps -> even)
-- True
-- >>> isEvenSwaps 2 1   -- e₂ * e₁ (1 swap -> odd)
-- False
-- >>> isEvenSwaps 11 5  -- e₁₂₄ * e₁₃ (3 swaps -> odd)
-- False
-- >>> isEvenSwaps 7 7   -- e₁₂₃ * e₁₂₃ (3 swaps -> odd)
-- False
isEvenSwaps :: Blade -> Blade -> Bool
isEvenSwaps _ 0 = True
isEvenSwaps 0 _ = True
isEvenSwaps b1 b2 = swapsToAdd == isEvenSwaps b1 b2' 
  where j = countTrailingZeros b2 
        swapsToAdd = even $ popCount (shiftR b1 (j + 1))
        b2' = clearBit b2 j 
{-# INLINE isEvenSwaps #-}

-- | Symmetric difference (combined blade) of two basis blades:
multBlades :: Blade -> Blade -> Blade
multBlades b1 b2 = b1 `xor` b2 -- need to add coef

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
    swapSign = if isEvenSwaps b1 b2 then 1 else -1

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
grade k (Clifford m) = Clifford (Map.filterWithKey (\b _ -> bladeGrade2 b == k) m)

-- | Decompose into a list of homogeneous grade components [Grade 0, Grade 1, ...]
grades :: forall p q r a. (KnownSignature p q r, Num a, Eq a) => Clifford p q r a -> [Clifford p q r a]
grades mv = [ grade k mv | k <- [0 .. totalDim (Proxy :: Proxy (Signature p q r))] ]

-- | Clifford Reversion (~A): reverses the order of basis vectors
--   For a homogeneous grade-k blade: ~⟨A⟩_k = (-1)^(k(k-1)/2) * ⟨A⟩_k
reverseCl :: (Num a, Eq a) => Clifford p q r a -> Clifford p q r a
reverseCl (Clifford m) =
  Clifford $ Map.mapWithKey (\b nz -> if even (gradeSignExp (bladeGrade2 b)) then nz else NonZeroUnsafe (negate (unNonZero nz))) m
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
    -- Grade condition: bladeGrade2(resBlade) == bladeGrade2(b1) + bladeGrade2(b2)
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
    , let r = bladeGrade2 b1
          s = bladeGrade2 b2
    , r <= s
    , let (resBlade, signVal) = multiplyBlades (Proxy :: Proxy (Signature p q r)) b1 b2
    , bladeGrade2 resBlade == (s - r)
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
    , let diff = abs (bladeGrade2 b1 - bladeGrade2 b2)
          (resBlade, signVal) = multiplyBlades (Proxy :: Proxy (Signature p q r)) b1 b2
    , bladeGrade2 resBlade == diff
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
    , let r = bladeGrade2 b1
          s = bladeGrade2 b2
    , r > 0 && s > 0
    , let diff = abs (r - s)
          (resBlade, signVal) = multiplyBlades (Proxy :: Proxy (Signature p q r)) b1 b2
    , bladeGrade2 resBlade == diff
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
      (\_ nz1 nz2 -> mkNonZero (unNonZero nz1 + unNonZero nz2))
      id
      id
      m1
      m2
  (*) = geometricProduct
  negate (Clifford m) = Clifford (Map.map (\nz -> NonZeroUnsafe (negate (unNonZero nz))) m)
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
