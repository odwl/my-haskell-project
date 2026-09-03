{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Lambda.Clifford.Universal
  ( -- * Core Universal Multivector Type  
    Clifford(..)
  , Blade
  , bladeGrade
  , bladeToIndices
  , indicesToBlade
  , basisBladeName
  , isEvenSwaps
  , multiplyBlades
  -- * Exact Scalar & Non-Zero Restriction
  , ExactScalar
  , NonZero(..)
  , pattern NonZero
  , mkNonZero
  , oneNZ
  -- * Constructors
  , zeroClifford
  , unitClifford
  , scalar
  , basis
  , basisIndex
  , blade
  , bladeNZ
  , fromBladeList
  , toBladeList
  , bladeCoeff
  , scalarCoeff
  -- * Geometric Operations
  , addClifford
  , negateClifford
  , grade
  , grades
  , reverseCl
  , filteredProduct
  , geometricProduct
  , wedge
  , scalarProd
  , cliffordScalar
  , leftContract
  , rightContract
  , fatDot
  , dotHestenes
  -- * Commutators & Symmetric Products
  , commutator
  , antiCommutator
  , symmetricProd
  , antiSymmetricProd
  -- * Forced Euclidean Contractions (Metric-Agnostic)
  , multiplyBladesEucl
  , filteredProductEucl
  , leftContractEucl
  , rightContractEucl
  , fatDotEucl
  , scalarProdEucl
  -- * Universal Property Homomorphism
  , universalFold
  -- * Infix Operators
  , (∼)
  , (∧)
  , (·)
  , (∗)
  , (⨼)
  , (⨽)
  , (●)
  , (•)
  , (⊙)
  , (×)
  ) where

import Control.Arrow ((>>>))
import Control.Monad (guard)
import Data.Bits (Bits(..), countTrailingZeros, popCount)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import Data.Proxy (Proxy(..))
import Data.Ratio (Ratio)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.TypeLits (KnownNat, Nat, natVal, type (+), type (<=))

import Lambda.Clifford.Signature

--------------------------------------------------------------------------------
-- Blade and NonZero
--------------------------------------------------------------------------------

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

-- | Scalar types with exact discrete zero representations (preventing IEEE 754 float drift in sparse maps).
class (Num a, Eq a) => ExactScalar a

instance ExactScalar Int
instance ExactScalar Integer
instance ExactScalar Word
instance ExactScalar Word8
instance ExactScalar Word16
instance ExactScalar Word32
instance ExactScalar Word64
instance ExactScalar Int8
instance ExactScalar Int16
instance ExactScalar Int32
instance ExactScalar Int64
instance ExactScalar (Ratio Integer)  -- Rational
instance ExactScalar (Ratio Int)
instance ExactScalar Float
instance ExactScalar Double

-- | A scalar coefficient guaranteed to NEVER be zero.
-- The constructor 'NonZeroUnsafe' is NOT exported to external users.
newtype NonZero a = NonZeroUnsafe { unNonZero :: a }
  deriving (Eq, Ord, Show, Read, Functor)

instance Applicative NonZero where
  pure = NonZeroUnsafe
  NonZeroUnsafe f <*> NonZeroUnsafe x = NonZeroUnsafe (f x)

instance Monad NonZero where
  NonZeroUnsafe x >>= f = f x

-- | Smart Constructor: Returns 'Nothing' if the value is zero.
mkNonZero :: ExactScalar a => a -> Maybe (NonZero a)
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

--------------------------------------------------------------------------------
-- Clifford
--------------------------------------------------------------------------------

-- | Universal Multivector parameterized by signature Cl(p, q, r) and scalar type a
newtype Clifford (p :: Nat) (q :: Nat) (r :: Nat) a = Clifford
  { unClifford :: Map Blade (NonZero a) }
  deriving (Eq)

-- | Construct a 1-vector basis element e_k from a dynamic runtime index (1 <= k <= n)
basisIndex :: forall p q r a. (KnownSignature p q r, ExactScalar a) => Int -> Clifford p q r a
basisIndex k
  | k >= 1 && k <= totalDim @p @q @r =
      bladeNZ (bit (k - 1)) oneNZ
  | otherwise = error $ "basisIndex: index " ++ show k ++ " out of range"
 
-- | Construct a single blade term: c * e_(indices)
blade :: ExactScalar a => Blade -> a -> Clifford p q r a
blade b c = case mkNonZero c of 
  Nothing -> Clifford Map.empty
  Just nz -> bladeNZ b nz

bladeNZ :: Blade -> NonZero a -> Clifford p q r a
bladeNZ b nz = Clifford (Map.singleton b nz)

-- | The additive zero multivector (0)
zeroClifford :: ExactScalar a => Clifford p q r a
zeroClifford = Clifford Map.empty

-- | The scalar unit multivector (1, multiplicative identity)
unitClifford :: ExactScalar a => Clifford p q r a
unitClifford = scalar 1

-- | Construct a pure Grade-0 scalar multivector
scalar :: forall p q r a. ExactScalar a => a -> Clifford p q r a
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
fromBladeList :: ExactScalar a => [(Blade, a)] -> Clifford p q r a
fromBladeList = Map.fromListWith (+) >>> Map.mapMaybe mkNonZero >>> Clifford

-- | Convert multivector to a sorted list of (Blade, coefficient) pairs
toBladeList :: Clifford p q r a -> [(Blade, a)]
toBladeList (Clifford m) = do
  (b, NonZero c) <- Map.toAscList m
  pure (b, c)

-- | Extract the scalar coefficient of a given basis blade (returns 0 if not present)
bladeCoeff :: Num a => Blade -> Clifford p q r a -> a
bladeCoeff b (Clifford m) = maybe 0 unNonZero (Map.lookup b m)

-- | Extract the Grade-0 scalar component as a raw number: ⟨A⟩₀
scalarCoeff :: Num a => Clifford p q r a -> a
scalarCoeff = bladeCoeff 0

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

-- | Num instance making Clifford an Associative Unital Ring
instance (KnownSignature p q r, ExactScalar a) => Num (Clifford p q r a) where
  (+) = addClifford
  (*) = geometricProduct
  negate = negateClifford
  abs _ = scalar 1 -- symbolic ring abs
  signum _ = scalar (fromInteger 1)
  fromInteger n = scalar (fromInteger n)

--------------------------------------------------------------------------------
-- Clifford Operations
--------------------------------------------------------------------------------

-- | Additive group addition of two multivectors (+):
--   Combines blade coefficients and suppresses cancelled zero terms.
addClifford :: ExactScalar a => Clifford p q r a -> Clifford p q r a -> Clifford p q r a
addClifford (Clifford m1) (Clifford m2) =
  Clifford (Map.mergeWithKey combine id id m1 m2)
  where
    combine _ (NonZeroUnsafe c1) (NonZeroUnsafe c2) = mkNonZero (c1 + c2)
{-# INLINE addClifford #-}

-- | Additive group negation of a multivector (-A):
--   Negates all blade coefficients.
negateClifford :: ExactScalar a => Clifford p q r a -> Clifford p q r a
negateClifford (Clifford m) = Clifford (Map.map (negate <$>) m)
{-# INLINE negateClifford #-}

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

-- | Direct basis blade multiplication under precomputed 'SignatureMasks':
--   Returns 'Just (resBlade, isPositive)', or 'Nothing' if annihilated in a degenerate subspace.
--
-- === Examples:
--
-- Euclidean 2D GA @Cl(2, 0, 0)@:
-- >>> multiplyBlades (SignatureMasks 0 0) 1 2    -- e₁ * e₂ = +e₁₂
-- Just (3,True)
-- >>> multiplyBlades (SignatureMasks 0 0) 2 1    -- e₂ * e₁ = -e₁₂
-- Just (3,False)
--
-- Quaternions @Cl(0, 2, 0)@ where @e₁² = -1, e₂² = -1@ (negMask = 0b11 = 3):
-- >>> multiplyBlades (SignatureMasks 3 0) 1 1    -- i² = -1
-- Just (0,False)
-- >>> multiplyBlades (SignatureMasks 3 0) 3 3    -- (e₁₂)² = -1
-- Just (0,False)
--
-- 3D Projective Geometric Algebra (PGA) @Cl(3, 0, 1)@ where @e₀² = 0@ (nullMask = 0b1000 = 8):
-- >>> multiplyBlades (SignatureMasks 0 8) 9 8    -- e₀₁ * e₀ = 0
-- Nothing
multiplyBlades :: SignatureMasks -> Blade -> Blade -> Maybe (Blade, Bool)
multiplyBlades (SignatureMasks negM nullM) b1 b2 
  | commonBits == 0    = Just (b1 .|. b2, isEvenSwap)
  | hasDegenerateBasis = Nothing
  | otherwise          = Just (xor b1 b2, isEvenMetric == isEvenSwap)
  where
    commonBits         = b1 .&. b2
    hasDegenerateBasis = (commonBits .&. nullM) /= 0
    isEvenMetric       = even (popCount (commonBits .&. negM))
    isEvenSwap         = isEvenSwaps b1 b2
{-# INLINE multiplyBlades #-}

-- | General blade-filtered geometric product.
--
-- All grade-projected and contracted products in Clifford algebra are instances of the
-- geometric product filtered by a bitwise blade predicate BEFORE metric evaluation:
--
-- * 'geometricProduct' (All grades):      @\_ _ -> True@
-- * 'leftContract'     (Grade s - r):     @\b1 b2 -> (b1 .&. b2) == b1@  (b1 ⊆ b2)
-- * 'rightContract'    (Grade r - s):     @\b1 b2 -> (b1 .&. b2) == b2@  (b2 ⊆ b1)
-- * 'fatDot'           (Grade |r - s|):   @\b1 b2 -> let c = b1 .&. b2 in c == b1 || c == b2@
filteredProduct :: forall p q r a. (KnownSignature p q r, ExactScalar a)
                => (Blade -> Blade -> Bool)
                -> Clifford p q r a
                -> Clifford p q r a
                -> Clifford p q r a
filteredProduct predicate (Clifford m1) (Clifford m2) =
  fromBladeList $ do
    (b1, NonZero c1) <- Map.toList m1
    (b2, NonZero c2) <- Map.toList m2
    guard (predicate b1 b2)
    (resBlade, isPos) <- maybeToList (multiplyBlades masks b1 b2)
    pure (resBlade, (if isPos then id else negate) (c1 * c2))
  where
    masks = signatureMasks (Proxy @(Signature p q r))
{-# INLINE filteredProduct #-}

-- | The Universal Geometric Product (*) on arbitrary dimensions
geometricProduct :: forall p q r a. (KnownSignature p q r, ExactScalar a)
                 => Clifford p q r a -> Clifford p q r a -> Clifford p q r a
geometricProduct = filteredProduct (\_ _ -> True)
{-# INLINE geometricProduct #-}

-- | Grade projection: extract all blade terms of homogeneous grade k (⟨A⟩ₖ).
--
-- === Examples:
--
-- >>> let mv = scalar 5 + basis @1 * 3 - basis @2 * 4 + (basis @1 * basis @2) * 7 :: Clifford 2 0 0 Int
-- >>> grade 0 mv
-- 5
-- >>> grade 1 mv
-- 3·e₁ + -4·e₂
-- >>> grade 2 mv
-- 7·e₁₂
-- >>> grade 3 mv
-- 0
grade :: ExactScalar a => Int -> Clifford p q r a -> Clifford p q r a
grade k (Clifford m) = Clifford (Map.filterWithKey (\b _ -> bladeGrade b == k) m)

-- | Decompose into a list of homogeneous grade components [Grade 0, Grade 1, ...]
grades :: forall p q r a. (KnownSignature p q r, ExactScalar a) => Clifford p q r a -> [Clifford p q r a]
grades mv = do
  k <- [0 .. totalDim @p @q @r]
  pure (grade k mv)

-- | Clifford Reversion (~A or Ã): the principal antiautomorphism reversing the order of basis 1-vectors in every blade.
--
-- For a product of vectors:
--
--   ~(v₁ v₂ ... vₖ) = vₖ ... v₂ v₁
--
-- For a homogeneous grade-k component, this introduces a sign factor:
--
--   ~⟨A⟩ₖ = (-1)^(k(k-1)/2) * ⟨A⟩ₖ
--
-- * Grade 0 (scalars):     +⟨A⟩₀  (invariant)
-- * Grade 1 (vectors):     +⟨A⟩₁  (invariant)
-- * Grade 2 (bivectors):   -⟨A⟩₂  (negated: ~(e₁ e₂) = e₂ e₁ = -e₁ e₂)
-- * Grade 3 (trivectors):  -⟨A⟩₃  (negated: ~(e₁ e₂ e₃) = e₃ e₂ e₁ = -e₁ e₂ e₃)
-- * Grade 4 (quadvectors): +⟨A⟩₄  (invariant)
--
-- Reversion satisfies the antiautomorphism property:
--
--   ~(A * B) = ~B * ~A
--
-- === Examples:
--
-- >>> let e1 = basis @1 :: Clifford 3 0 0 Int
-- >>> let e2 = basis @2 :: Clifford 3 0 0 Int
-- >>> let e3 = basis @3 :: Clifford 3 0 0 Int
--
-- Reversing a scalar or vector leaves it unchanged:
-- >>> reverseCl (scalar 5)
-- 5
-- >>> reverseCl (3 * e1 + 4 * e2)
-- 3·e₁ + 4·e₂
--
-- Reversing a bivector or trivector flips its sign:
-- >>> reverseCl (e1 * e2)
-- -1·e₁₂
-- >>> reverseCl (e1 * e2 * e3)
-- -1·e₁₂₃
--
-- Reversing a mixed multivector:
-- >>> let mv = scalar 5 + basis @1 * 3 - basis @2 * 4 + (basis @1 * basis @2) * 7 :: Clifford 2 0 0 Int
-- >>> reverseCl mv
-- 5 + 3·e₁ + -4·e₂ + -7·e₁₂
reverseCl :: ExactScalar a => Clifford p q r a -> Clifford p q r a
reverseCl (Clifford m) = Clifford (Map.mapWithKey reverseBladeTerm m)
  where
    reverseBladeTerm b nz
      | isEvenReversion (bladeGrade b) = nz
      | otherwise                      = negate <$> nz

    -- Reversion sign (-1)^(k(k-1)/2) has 4-periodicity (+1, +1, -1, -1):
    -- Bit 1 of grade k is 0 for grades [0, 1, 4, 5...] and 1 for grades [2, 3, 6, 7...]
    isEvenReversion = (== 0) . (.&. 2)

-- | Exterior / Wedge Product (∧): Grade-summing outer product
--   ⟨A⟩_r ∧ ⟨B⟩_s = ⟨A * B⟩_(r+s)
wedge :: forall p q r a. (KnownSignature p q r, ExactScalar a)
      => Clifford p q r a -> Clifford p q r a -> Clifford p q r a
wedge (Clifford m1) (Clifford m2) =
  fromBladeList $ do
    (b1, NonZero c1) <- Map.toList m1
    (b2, NonZero c2) <- Map.toList m2
    -- Grade condition: bladeGrade(resBlade) == bladeGrade(b1) + bladeGrade(b2)
    -- This happens iff b1 and b2 share NO common basis vectors: (b1 .&. b2) == 0
    guard (b1 .&. b2 == 0)
    pure (b1 .|. b2, (if isEvenSwaps b1 b2 then id else negate) (c1 * c2))
{-# INLINE wedge #-}

-- | Scalar Product (⟨A * B⟩₀): extracts Grade-0 scalar component of geometric product
scalarProd :: (KnownSignature p q r, ExactScalar a) => Clifford p q r a -> Clifford p q r a -> a
scalarProd a b = scalarCoeff (geometricProduct a b)

-- | Clifford Scalar Product (⟨A * ~B⟩₀): strictly positive-definite Hilbert inner product
cliffordScalar :: (KnownSignature p q r, ExactScalar a) => Clifford p q r a -> Clifford p q r a -> a
cliffordScalar a b = scalarProd a (reverseCl b)

-- | Left Contraction (⨼ / ⌋):
--   ⟨A⟩_r ⨼ ⟨B⟩_s = ⟨A * B⟩_(s - r)  when r <= s
--   ⟨A⟩_r ⨼ ⟨B⟩_s = 0                when r > s
--
-- Optimization: grade(resBlade) == s - r iff b1 is a bitwise submask of b2 ((b1 .&. b2) == b1).
leftContract :: forall p q r a. (KnownSignature p q r, ExactScalar a)
             => Clifford p q r a -> Clifford p q r a -> Clifford p q r a
leftContract = filteredProduct (\b1 b2 -> (b1 .&. b2) == b1)
{-# INLINE leftContract #-}

-- | Right Contraction (⨽ / ⌊):
--   ⟨A⟩_r ⨽ ⟨B⟩_s = ⟨A * B⟩_(r - s)  when r >= s
--   ⟨A⟩_r ⨽ ⟨B⟩_s = 0                when r < s
--
-- Optimization: grade(resBlade) == r - s iff b2 is a bitwise submask of b1 ((b1 .&. b2) == b2).
rightContract :: forall p q r a. (KnownSignature p q r, ExactScalar a)
              => Clifford p q r a -> Clifford p q r a -> Clifford p q r a
rightContract = filteredProduct (\b1 b2 -> (b1 .&. b2) == b2)
{-# INLINE rightContract #-}

-- | Fat Dot Product (●): ⟨A⟩_r ● ⟨B⟩_s = ⟨A * B⟩_|r - s| for all grades r, s >= 0
--
-- Optimization: grade(resBlade) == |r - s| iff one blade is a bitwise submask of the other.
fatDot :: forall p q r a. (KnownSignature p q r, ExactScalar a)
       => Clifford p q r a -> Clifford p q r a -> Clifford p q r a
fatDot = filteredProduct (\b1 b2 -> let common = b1 .&. b2 in common == b1 || common == b2)
{-# INLINE fatDot #-}

-- | Hestenes Inner Product (•): ⟨A⟩_r • ⟨B⟩_s = ⟨A * B⟩_|r - s| (for r, s > 0, scalars drop to 0)
--
-- Expressed cleanly as 'fatDot' after stripping Grade-0 (scalar) basis components.
dotHestenes :: (KnownSignature p q r, ExactScalar a)
            => Clifford p q r a -> Clifford p q r a -> Clifford p q r a
dotHestenes (Clifford m1) (Clifford m2) =
  fatDot (Clifford (Map.delete 0 m1)) (Clifford (Map.delete 0 m2))
{-# INLINE dotHestenes #-}

-- | Commutator product (Lie bracket): [A, B] = A * B - B * A
--   Measures the algebraic non-commutativity of two multivectors.
commutator :: (KnownSignature p q r, ExactScalar a)
           => Clifford p q r a -> Clifford p q r a -> Clifford p q r a
commutator a b = (a * b) - (b * a)
{-# INLINE commutator #-}

-- | Anti-commutator product (Jordan bracket): {A, B} = A * B + B * A
antiCommutator :: (KnownSignature p q r, ExactScalar a)
               => Clifford p q r a -> Clifford p q r a -> Clifford p q r a
antiCommutator a b = (a * b) + (b * a)
{-# INLINE antiCommutator #-}

-- | Semi-Symmetric (Jordan) Product (⊙): A ⊙ B = 1/2 (A * B + B * A)
--   Symmetric component of the geometric product. On 1-vectors u, v: u ⊙ v = u · v.
symmetricProd :: (KnownSignature p q r, Fractional a, ExactScalar a)
              => Clifford p q r a -> Clifford p q r a -> Clifford p q r a
symmetricProd a b = antiCommutator a b * scalar (1 / 2)
{-# INLINE symmetricProd #-}

-- | Semi-Antisymmetric (Commutator Cross) Product (×): A × B = 1/2 (A * B - B * A)
--   Antisymmetric component of the geometric product. On 1-vectors u, v: u × v = u ∧ v.
antiSymmetricProd :: (KnownSignature p q r, Fractional a, ExactScalar a)
                  => Clifford p q r a -> Clifford p q r a -> Clifford p q r a
antiSymmetricProd a b = commutator a b * scalar (1 / 2)
{-# INLINE antiSymmetricProd #-}

--------------------------------------------------------------------------------
-- Forced Euclidean Contractions (Metric-Agnostic)
--------------------------------------------------------------------------------

-- | Direct basis blade multiplication under a forced positive-definite Euclidean metric (all eᵢ² = +1).
--   Ignores negative and null metric signatures, treating every basis blade as Euclidean.
multiplyBladesEucl :: Blade -> Blade -> (Blade, Bool)
multiplyBladesEucl b1 b2 = (xor b1 b2, isEvenSwaps b1 b2)
{-# INLINE multiplyBladesEucl #-}

-- | Forced Euclidean blade-filtered product:
--   Evaluates multivector products assuming all basis vectors satisfy eᵢ² = +1.
filteredProductEucl :: ExactScalar a
                    => (Blade -> Blade -> Bool)
                    -> Clifford p q r a
                    -> Clifford p q r a
                    -> Clifford p q r a
filteredProductEucl predicate (Clifford m1) (Clifford m2) =
  fromBladeList $ do
    (b1, NonZero c1) <- Map.toList m1
    (b2, NonZero c2) <- Map.toList m2
    guard (predicate b1 b2)
    let (resBlade, isPos) = multiplyBladesEucl b1 b2
    pure (resBlade, (if isPos then id else negate) (c1 * c2))
{-# INLINE filteredProductEucl #-}

-- | Forced Euclidean Left Contraction (⨼!):
--   Computes left contraction as if the underlying metric were strictly Euclidean (all eᵢ² = +1).
--   Essential in Projective Geometric Algebra (PGA) for metric-independent meet/join operations.
leftContractEucl :: ExactScalar a => Clifford p q r a -> Clifford p q r a -> Clifford p q r a
leftContractEucl = filteredProductEucl (\b1 b2 -> (b1 .&. b2) == b1)
{-# INLINE leftContractEucl #-}

-- | Forced Euclidean Right Contraction (⨽!):
--   Computes right contraction as if the underlying metric were strictly Euclidean (all eᵢ² = +1).
rightContractEucl :: ExactScalar a => Clifford p q r a -> Clifford p q r a -> Clifford p q r a
rightContractEucl = filteredProductEucl (\b1 b2 -> (b1 .&. b2) == b2)
{-# INLINE rightContractEucl #-}

-- | Forced Euclidean Fat Dot Product (●!):
--   Computes symmetric grade-difference product under a forced Euclidean metric.
fatDotEucl :: ExactScalar a => Clifford p q r a -> Clifford p q r a -> Clifford p q r a
fatDotEucl = filteredProductEucl (\b1 b2 -> let common = b1 .&. b2 in common == b1 || common == b2)
{-# INLINE fatDotEucl #-}

-- | Forced Euclidean Scalar Product (·!):
--   Extracts the Grade-0 scalar product under a forced Euclidean metric: ⟨A *ₑᵤ꜀ B⟩₀.
scalarProdEucl :: ExactScalar a => Clifford p q r a -> Clifford p q r a -> a
scalarProdEucl a b = scalarCoeff (filteredProductEucl (\_ _ -> True) a b)
{-# INLINE scalarProdEucl #-}

-- | The Universal Property Homomorphism (Universal Fold):
--   Given an evaluation function for 1-vectors in an algebra `evalBasis :: Int -> alg`,
--   uniquely extends to evaluate arbitrary multivectors!
universalFold :: forall p q r a alg. (KnownSignature p q r, Num alg)
              => (Int -> alg) -> (a -> alg) -> Clifford p q r a -> alg
universalFold evalBasis evalScalar (Clifford m) =
  sum [ evalScalar c * evalBlade b | (b, NonZero c) <- Map.toList m ]
  where
    evalBlade 0 = 1
    evalBlade b = product [ evalBasis (i + 1) | i <- setBitIndices b ]
    
    setBitIndices 0 = []
    setBitIndices msk =
      let i = countTrailingZeros msk
      in i : setBitIndices (msk .&. (msk - 1))

--------------------------------------------------------------------------------
-- Infix & Prefix Operators
--------------------------------------------------------------------------------
infix 8 ∼
-- | Clifford Reversion Operator (∼): ~⟨A⟩ₖ = (-1)^(k(k-1)/2) · ⟨A⟩ₖ
(∼) :: ExactScalar a => Clifford p q r a -> Clifford p q r a
(∼) = reverseCl

infixl 7 ∧
(∧) :: (KnownSignature p q r, ExactScalar a) => Clifford p q r a -> Clifford p q r a -> Clifford p q r a
(∧) = wedge

infixl 7 ·
(·) :: (KnownSignature p q r, ExactScalar a) => Clifford p q r a -> Clifford p q r a -> Clifford p q r a
(·) a b = scalar (scalarProd a b)

infixl 7 ∗
(∗) :: (KnownSignature p q r, ExactScalar a) => Clifford p q r a -> Clifford p q r a -> Clifford p q r a
(∗) a b = scalar (cliffordScalar a b)

infixl 7 ⨼
(⨼) :: (KnownSignature p q r, ExactScalar a) => Clifford p q r a -> Clifford p q r a -> Clifford p q r a
(⨼) = leftContract

infixl 7 ⨽
(⨽) :: (KnownSignature p q r, ExactScalar a) => Clifford p q r a -> Clifford p q r a -> Clifford p q r a
(⨽) = rightContract

infixl 7 ●
(●) :: (KnownSignature p q r, ExactScalar a) => Clifford p q r a -> Clifford p q r a -> Clifford p q r a
(●) = fatDot

infixl 7 •
(•) :: (KnownSignature p q r, ExactScalar a) => Clifford p q r a -> Clifford p q r a -> Clifford p q r a
(•) = dotHestenes

infixl 7 ⊙
-- | Semi-Symmetric (Jordan) Product Operator: A ⊙ B = 1/2 (A * B + B * A)
(⊙) :: (KnownSignature p q r, Fractional a, ExactScalar a) => Clifford p q r a -> Clifford p q r a -> Clifford p q r a
(⊙) = symmetricProd

infixl 7 ×
-- | Semi-Antisymmetric (Commutator Cross) Product Operator: A × B = 1/2 (A * B - B * A)
(×) :: (KnownSignature p q r, Fractional a, ExactScalar a) => Clifford p q r a -> Clifford p q r a -> Clifford p q r a
(×) = antiSymmetricProd
