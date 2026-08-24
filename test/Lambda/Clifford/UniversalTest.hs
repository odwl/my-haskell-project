{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-simplifiable-class-constraints #-}

module Lambda.Clifford.UniversalTest (universalCliffordTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding ((.&.))
import Data.Bits (shiftL, complement, (.&.))
import Data.Proxy (Proxy(..))
import qualified Data.Map.Strict as Map
import Data.List (sort, nub)
import GHC.TypeLits (KnownNat)
import Lambda.Clifford.Signature
import Lambda.Clifford.Universal

-- | QuickCheck Arbitrary generator for Clifford multivectors
instance (KnownNat p, KnownNat q, KnownNat r, Arbitrary a, Num a, Eq a) => Arbitrary (Clifford p q r a) where
  arbitrary = do
    let n = totalDim (Proxy :: Proxy (Signature p q r))
        maxBlade = (1 `shiftL` n) - 1
    coeffs <- vectorOf (fromIntegral (maxBlade + 1)) arbitrary
    return $ fromBladeList (zip [0 .. maxBlade] coeffs)

universalCliffordTests :: TestTree
universalCliffordTests = testGroup "Universal Clifford Algebra Tests"
  [ testGroup "Signature & Basis Squares"
      [ testCase "G² Cl(2,0) Euclidean basis squares" $ do
          let e1 = basis @1 :: Clifford 2 0 0 Int
              e2 = basis @2 :: Clifford 2 0 0 Int
          e1 * e1 @?= 1 
          e2 * e2 @?= 1
          e1 * e2 @?= blade 3 1
          e2 * e1 @?= blade 3 (-1)
          e1 * e2 + e2 * e1 @?= 0

      , testCase "Quaternions Cl(0,2) basis squares i² = j² = k² = -1" $ do
          let i = basis @1 :: Clifford 0 2 0 Int
              j = basis @2 :: Clifford 0 2 0 Int
              k = i * j
          i * i @?= -1
          j * j @?= -1
          k * k @?= -1
          i * j * k @?= -1

      , testCase "Minkowski (1+1)D Cl(1,1) basis squares" $ do
          let et = basis @1 :: Clifford 1 1 0 Int
              ex = basis @2 :: Clifford 1 1 0 Int
          et * et @?= 1
          ex * ex @?= -1
          et * ex + ex * et @?= 0

      , testCase "Dirac STA Cl(1,3) basis squares" $ do
          let gamma0 = basis @1 :: Clifford 1 3 0 Int
              gamma1 = basis @2 :: Clifford 1 3 0 Int
              gamma2 = basis @3 :: Clifford 1 3 0 Int
              gamma3 = basis @4 :: Clifford 1 3 0 Int
          gamma0 * gamma0 @?= 1
          gamma1 * gamma1 @?= -1
          gamma2 * gamma2 @?= -1
          gamma3 * gamma3 @?= -1

      , testCase "PGA Cl(3,0,1) degenerate blade e0² = 0" $ do
          let e1 = basis @1 :: Clifford 3 0 1 Int
              e2 = basis @2 :: Clifford 3 0 1 Int
              e3 = basis @3 :: Clifford 3 0 1 Int
              e0 = basis @4 :: Clifford 3 0 1 Int
          e1 * e1 @?= 1
          e2 * e2 @?= 1
          e3 * e3 @?= 1
          e0 * e0 @?= 0
      ]

  , testGroup "QuickCheck Ring & Algebra Axioms"
      [ testProperty "Cl(2,0) Geometric Product Associativity: (A * B) * C = A * (B * C)" $
          \(a :: Clifford 2 0 0 Int) b c -> (a * b) * c == a * (b * c)

      , testProperty "Cl(3,0) Geometric Product Associativity: (A * B) * C = A * (B * C)" $
          \(a :: Clifford 3 0 0 Int) b c -> (a * b) * c == a * (b * c)

      , testProperty "Cl(1,3) STA Geometric Product Associativity: (A * B) * C = A * (B * C)" $
          \(a :: Clifford 1 3 0 Int) b c -> (a * b) * c == a * (b * c)

      , testProperty "Unital Identity: 1 * A = A and A * 1 = A" $
          \(a :: Clifford 2 0 0 Int) -> 1 * a == a && a * 1 == a

      , testProperty "Left Distributivity: A * (B + C) = A*B + A*C" $
          \(a :: Clifford 2 0 0 Int) b c -> a * (b + c) == a * b + a * c

      , testProperty "Right Distributivity: (A + B) * C = A*C + B*C" $
          \(a :: Clifford 2 0 0 Int) b c -> (a + b) * c == a * c + b * c
      ]

  , testGroup "Products & Duality Laws"
      [ testProperty "Wedge Anticommutativity on Vectors: u ∧ v = -(v ∧ u)" $
          \(x1 :: Int) y1 x2 y2 ->
            let u = blade 1 x1 + blade 2 y1 :: Clifford 2 0 0 Int
                v = blade 1 x2 + blade 2 y2 :: Clifford 2 0 0 Int
            in (u ∧ v) == negate (v ∧ u)

      , testProperty "Geometric Product Decomposition on Vectors: u * v = (u · v) + (u ∧ v)" $
          \(x1 :: Int) y1 x2 y2 ->
            let u = blade 1 x1 + blade 2 y1 :: Clifford 2 0 0 Int
                v = blade 1 x2 + blade 2 y2 :: Clifford 2 0 0 Int
            in u * v == (u · v) + (u ∧ v)

      , testProperty "Left Contraction Adjoint Duality: ⟨(A ∧ B) * C⟩₀ = ⟨A * (B ⨼ C)⟩₀" $
          \(a :: Clifford 2 0 0 Int) b c ->
            scalarProd (a ∧ b) c == scalarProd a (b ⨼ c)

      , testProperty "Reversion Antiautomorphism: ~(A * B) = ~B * ~A" $
          \(a :: Clifford 2 0 0 Int) b ->
            reverseCl (a * b) == reverseCl b * reverseCl a
      ]

  , testGroup "Universal Property Homomorphism"
      [ testCase "Evaluates multivector in a custom algebra" $ do
          -- In Cl(2,0), test universal fold with scalar evaluation
          let mv = scalar 5 + basis @1 * 2 + basis @2 * 3 + (basis @1 * basis @2) * 4 :: Clifford 2 0 0 Int
              -- Evaluate basis vectors as integers e1 -> 10, e2 -> 100
              evalBasis 1 = 10
              evalBasis 2 = 100
              evalBasis _ = 0
              res = universalFold evalBasis id mv
          -- 5 + 2*10 + 3*100 + 4*(10*100) = 5 + 20 + 300 + 4000 = 4325
          res @?= 4325
      ]

  , testGroup "Canonical Zero-Suppression & Invalid Map Invariants"
      [ testCase "Raw Clifford with explicit 0 entries breaks Eq against canonical 0" $ do
          let canonicalZero     = scalar 0 :: Clifford 2 0 0 Int
              invalidZeroScalar = Clifford (Map.singleton 0 (NonZeroUnsafe 0)) :: Clifford 2 0 0 Int
              invalidZeroVector = Clifford (Map.singleton 1 (NonZeroUnsafe 0)) :: Clifford 2 0 0 Int
              invalidZeroMulti  = Clifford (Map.fromList [(0, NonZeroUnsafe 0), (1, NonZeroUnsafe 0), (3, NonZeroUnsafe 0)]) :: Clifford 2 0 0 Int
          -- Demonstrates why non-canonical maps with explicit 0 values fail equality:
          (invalidZeroScalar == canonicalZero) @?= False
          (invalidZeroVector == canonicalZero) @?= False
          (invalidZeroMulti  == canonicalZero) @?= False

      , testCase "fromBladeList always cleans up 0 entries into canonical Map.empty" $ do
          let canonicalZero = scalar 0 :: Clifford 2 0 0 Int
              cleaned = fromBladeList [(0, 0), (1, 0), (3, 0)] :: Clifford 2 0 0 Int
          cleaned @?= canonicalZero
          unClifford cleaned @?= Map.empty

      , testCase "Cancellation via subtraction (a - a) produces canonical Map.empty" $ do
          let v = (basis @1 * 5 + basis @2 * 3) :: Clifford 2 0 0 Int
              diff = v - v
          diff @?= scalar 0
          unClifford diff @?= Map.empty
      ]

  , testGroup "Blade List Invariants & Round-Trip"
      [ testProperty "Cl(2,0) Round-Trip: fromBladeList (toBladeList v) == v" $
          \(v :: Clifford 2 0 0 Int) -> fromBladeList (toBladeList v) == v

      , testProperty "Cl(3,0) Round-Trip: fromBladeList (toBladeList v) == v" $
          \(v :: Clifford 3 0 0 Int) -> fromBladeList (toBladeList v) == v

      , testProperty "Cl(1,3) STA Round-Trip: fromBladeList (toBladeList v) == v" $
          \(v :: Clifford 1 3 0 Int) -> fromBladeList (toBladeList v) == v

      , testProperty "Cl(3,0,1) PGA Round-Trip: fromBladeList (toBladeList v) == v" $
          \(v :: Clifford 3 0 1 Int) -> fromBladeList (toBladeList v) == v

      , testCase "Concrete Round-Trip on inhomogeneous multivector" $ do
          let v = scalar 5 + basis @1 * 3 - basis @2 * 4 + (basis @1 * basis @2) * 7 :: Clifford 2 0 0 Int
              bladeList = toBladeList v
          fromBladeList bladeList @?= v

      , testProperty "QuickCheck Isomorphism 1: indicesToBlade (bladeToIndices b) == b" $
          \(b :: Blade) -> indicesToBlade (bladeToIndices b) == b

      , testProperty "QuickCheck Isomorphism 2: bladeToIndices (indicesToBlade is) == is" $
          \(isRaw :: [Positive Int]) ->
            let is = nub (sort [ x | Positive x <- isRaw, x <= 64 ])
            in bladeToIndices (indicesToBlade is) == is

      , testCase "bladeToIndices and indicesToBlade isomorphism unit tests" $ do
          bladeToIndices 0 @?= []
          indicesToBlade [] @?= 0
          bladeToIndices 1 @?= [1]
          indicesToBlade [1] @?= 1
          bladeToIndices 3 @?= [1, 2]
          indicesToBlade [1, 2] @?= 3
          bladeToIndices 7 @?= [1, 2, 3]
          indicesToBlade [1, 2, 3] @?= 7
          let b = 13 :: Blade -- 0b1101 = [1, 3, 4]
          indicesToBlade (bladeToIndices b) @?= b
      ]

  , testGroup "Blade Swap Parity (isEvenSwaps) Invariants"
      [ testProperty "Invariant 1 (Empty Blade Identity): isEvenSwaps b 0 == True and isEvenSwaps 0 b == True" $
          \(b :: Blade) -> isEvenSwaps b 0 && isEvenSwaps 0 b

      , testProperty "Invariant 2 (Self-Swaps Parity): isEvenSwaps b b == even (k * (k - 1) `div` 2)" $
          \(b :: Blade) ->
            let k = bladeGrade2 b
            in isEvenSwaps b b == even ((k * (k - 1)) `div` 2)

      , testProperty "Invariant 3 (Complementarity on Disjoint Blades): isEvenSwaps b1 b2 == (even (k1 * k2) == isEvenSwaps b2 b1)" $
          \(b1 :: Blade) (b2Raw :: Blade) ->
            let b2 = b2Raw .&. complement b1  -- ensure b1 and b2 are disjoint (b1 .&. b2 == 0)
                k1 = bladeGrade2 b1
                k2 = bladeGrade2 b2
            in isEvenSwaps b1 b2 == (even (k1 * k2) == isEvenSwaps b2 b1)

      , testProperty "Invariant 4 (List Inversion Parity Equivalence): isEvenSwaps matches even of list inversions" $
          \(b1 :: Blade) (b2 :: Blade) ->
            let is = bladeToIndices b1
                js = bladeToIndices b2
                expectedEven = even (sum [ 1 | i <- is, j <- js, i > j ])
            in isEvenSwaps b1 b2 == expectedEven

      , testCase "Concrete isEvenSwaps unit tests" $ do
          isEvenSwaps 0 0 @?= True   -- 0 swaps -> True
          isEvenSwaps 1 2 @?= True   -- e₁ * e₂ (0 swaps -> True)
          isEvenSwaps 2 1 @?= False  -- e₂ * e₁ (1 swap -> False)
          isEvenSwaps 11 5 @?= False -- e₁₂₄ * e₁₃ (3 swaps -> False)
          isEvenSwaps 7 7 @?= False  -- e₁₂₃ * e₁₂₃ (3 swaps -> False)
          isEvenSwaps 3 3 @?= False  -- e₁₂ * e₁₂ (1 swap -> False)
          isEvenSwaps 15 15 @?= True -- e₁₂₃₄ * e₁₂₃₄ (6 swaps -> True)
      ]

  , testGroup "Show Formatting Tests"
      [ testCase "Zero multivector shows as 0" $ do
          let z = scalar 0 :: Clifford 2 0 0 Int
          show z @?= "0"

      , testCase "Pure scalar shows as number" $ do
          let s = scalar 5 :: Clifford 2 0 0 Int
          show s @?= "5"

      , testCase "Pure basis vector shows with unicode subscript" $ do
          let e1 = basis @1 :: Clifford 2 0 0 Int
          show e1 @?= "1·e₁"
          let e2 = basis @2 * 3 :: Clifford 2 0 0 Int
          show e2 @?= "3·e₂"

      , testCase "Inhomogeneous multivector shows with + and · separators" $ do
          let v = scalar 5 + basis @1 * 3 - basis @2 * 4 + (basis @1 * basis @2) * 7 :: Clifford 2 0 0 Int
          show v @?= "5 + 3·e₁ + -4·e₂ + 7·e₁₂"

      , testCase "4D Spacetime STA multivector shows higher basis blades correctly" $ do
          let sta = basis @1 * 2 + (basis @2 * basis @3 * basis @4) * 9 :: Clifford 1 3 0 Int
          show sta @?= "2·e₁ + 9·e₂₃₄"
      ]
  ]
