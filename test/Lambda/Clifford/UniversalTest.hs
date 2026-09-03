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
import Data.Bits (shiftL, complement, bit, (.&.), popCount)
import Data.Proxy (Proxy(..))
import Data.Ratio ((%))
import qualified Data.Map.Strict as Map
import Data.List (sort, nub)
import Lambda.Clifford.Signature
import Lambda.Clifford.Universal

-- | QuickCheck Arbitrary generator for Clifford multivectors
instance (KnownSignature p q r, Arbitrary a, ExactScalar a) => Arbitrary (Clifford p q r a) where
  arbitrary = do
    let n = totalDim @p @q @r
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

      , testProperty "Left Contraction STA Cl(1,3) Duality: ⟨(A ∧ B) * C⟩₀ = ⟨A * (B ⨼ C)⟩₀" $
          \(a :: Clifford 1 3 0 Int) b c ->
            scalarProd (a ∧ b) c == scalarProd a (b ⨼ c)

      , testProperty "Left Contraction Grade Condition: b1 ⨼ b2 == (if r <= s then grade (s - r) (b1 * b2) else 0)" $
          forAll (choose (0, 15 :: Word)) $ \b1 ->
          forAll (choose (0, 15 :: Word)) $ \b2 ->
            let v1 = blade b1 1 :: Clifford 1 3 0 Int
                v2 = blade b2 1 :: Clifford 1 3 0 Int
                r  = bladeGrade b1
                s  = bladeGrade b2
                expected = if r <= s then grade (s - r) (v1 * v2) else 0
            in (v1 ⨼ v2) == expected

      , testProperty "Graded Leibniz Derivation on Vectors & Multivector Cl(3,0): a ⨼ (b ∧ C) == (a ⨼ b) ∧ C - b ∧ (a ⨼ C)" $
          \(aRaw :: Clifford 3 0 0 Int) bRaw (c :: Clifford 3 0 0 Int) ->
            let a = grade 1 aRaw
                b = grade 1 bRaw
            in (a ⨼ (b ∧ c)) == ((a ⨼ b) ∧ c - b ∧ (a ⨼ c))

      , testProperty "Graded Leibniz Derivation on Vectors & Multivector STA Cl(1,3): a ⨼ (b ∧ C) == (a ⨼ b) ∧ C - b ∧ (a ⨼ C)" $
          \(aRaw :: Clifford 1 3 0 Int) bRaw (c :: Clifford 1 3 0 Int) ->
            let a = grade 1 aRaw
                b = grade 1 bRaw
            in (a ⨼ (b ∧ c)) == ((a ⨼ b) ∧ c - b ∧ (a ⨼ c))

      , testProperty "Associativity of Wedge and Left Contraction Cl(3,0): (A ∧ B) ⨼ C == A ⨼ (B ⨼ C)" $
          \(a :: Clifford 3 0 0 Int) b c ->
            ((a ∧ b) ⨼ c) == (a ⨼ (b ⨼ c))

      , testProperty "Associativity of Wedge and Left Contraction STA Cl(1,3): (A ∧ B) ⨼ C == A ⨼ (B ⨼ C)" $
          \(a :: Clifford 1 3 0 Int) b c ->
            ((a ∧ b) ⨼ c) == (a ⨼ (b ⨼ c))

      , testProperty "Right Contraction Adjoint Duality: ⟨A * (B ∧ C)⟩₀ = ⟨(A ⨽ B) * C⟩₀" $
          \(a :: Clifford 2 0 0 Int) b c ->
            scalarProd a (b ∧ c) == scalarProd (a ⨽ b) c

      , testProperty "Right Contraction STA Cl(1,3) Duality: ⟨A * (B ∧ C)⟩₀ = ⟨(A ⨽ B) * C⟩₀" $
          \(a :: Clifford 1 3 0 Int) b c ->
            scalarProd a (b ∧ c) == scalarProd (a ⨽ b) c

      , testProperty "Right Contraction Grade Condition: b1 ⨽ b2 == (if r >= s then grade (r - s) (b1 * b2) else 0)" $
          forAll (choose (0, 15 :: Word)) $ \b1 ->
          forAll (choose (0, 15 :: Word)) $ \b2 ->
            let v1 = blade b1 1 :: Clifford 1 3 0 Int
                v2 = blade b2 1 :: Clifford 1 3 0 Int
                r  = bladeGrade b1
                s  = bladeGrade b2
                expected = if r >= s then grade (r - s) (v1 * v2) else 0
            in (v1 ⨽ v2) == expected

      , testProperty "Fat Dot Grade Condition: b1 ● b2 == grade |r - s| (b1 * b2)" $
          forAll (choose (0, 15 :: Word)) $ \b1 ->
          forAll (choose (0, 15 :: Word)) $ \b2 ->
            let v1 = blade b1 1 :: Clifford 1 3 0 Int
                v2 = blade b2 1 :: Clifford 1 3 0 Int
                r  = bladeGrade b1
                s  = bladeGrade b2
                expected = grade (abs (r - s)) (v1 * v2)
            in (v1 ● v2) == expected

      , testProperty "Hestenes Dot Grade Condition: b1 • b2 == (if r > 0 && s > 0 then grade |r - s| (b1 * b2) else 0)" $
          forAll (choose (0, 15 :: Word)) $ \b1 ->
          forAll (choose (0, 15 :: Word)) $ \b2 ->
            let v1 = blade b1 1 :: Clifford 1 3 0 Int
                v2 = blade b2 1 :: Clifford 1 3 0 Int
                r  = bladeGrade b1
                s  = bladeGrade b2
                expected = if r > 0 && s > 0 then grade (abs (r - s)) (v1 * v2) else 0
            in (v1 • v2) == expected

      , testProperty "Fat Dot Decomposition Cl(2,0): A ● B == (A ⨼ B) + (A ⨽ B) - (A · B)" $
          \(a :: Clifford 2 0 0 Int) b ->
            (a ● b) == (a ⨼ b) + (a ⨽ b) - (a · b)

      , testProperty "Fat Dot Decomposition STA Cl(1,3): A ● B == (A ⨼ B) + (A ⨽ B) - (A · B)" $
          \(a :: Clifford 1 3 0 Int) b ->
            (a ● b) == (a ⨼ b) + (a ⨽ b) - (a · b)

      , testProperty "Contraction Reversion Duality Cl(2,0): A ⨽ B == ~(~B ⨼ ~A)" $
          \(a :: Clifford 2 0 0 Int) b ->
            (a ⨽ b) == reverseCl (reverseCl b ⨼ reverseCl a)

      , testProperty "Contraction Reversion Duality STA Cl(1,3): A ⨽ B == ~(~B ⨼ ~A)" $
          \(a :: Clifford 1 3 0 Int) b ->
            (a ⨽ b) == reverseCl (reverseCl b ⨼ reverseCl a)

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

  , testGroup "Exact Rational Arithmetic Cl(p,q,r) Rational"
      [ testCase "Rational fractional addition, subtraction, and exact zero cancellation" $ do
          let v1 = basis @1 * scalar (1 % 3) + basis @2 * scalar (2 % 3) :: Clifford 2 0 0 Rational
              v2 = basis @1 * scalar (1 % 3) + basis @2 * scalar (2 % 3) :: Clifford 2 0 0 Rational
              diff = v1 - v2
          diff @?= 0
          unClifford diff @?= Map.empty

      , testCase "Rational wedge and contraction on fractional multivectors" $ do
          let e1 = basis @1 :: Clifford 2 0 0 Rational
              e2 = basis @2 :: Clifford 2 0 0 Rational
              u = e1 * scalar (1 % 2)
              v = e2 * scalar (2 % 3)
              plane = u ∧ v
          plane @?= (blade 3 (1 % 3) :: Clifford 2 0 0 Rational)
          (u ⨼ plane) @?= (e2 * scalar (1 % 6) :: Clifford 2 0 0 Rational)
          (plane ⨽ v) @?= (e1 * scalar (2 % 9) :: Clifford 2 0 0 Rational)
      ]

  , testGroup "Commutator and Semi-Symmetric (Jordan) Products"
      [ testProperty "Commutator Anti-Symmetry: commutator A B == -(commutator B A)" $
          \(a :: Clifford 2 0 0 Int) b -> commutator a b == negate (commutator b a)

      , testProperty "Anti-Commutator Symmetry: antiCommutator A B == antiCommutator B A" $
          \(a :: Clifford 2 0 0 Int) b -> antiCommutator a b == antiCommutator b a

      , testProperty "Jordan-Lie Decomposition: A * B == (A ⊙ B) + (A × B)" $
          \(a :: Clifford 2 0 0 Rational) b -> (a ⊙ b) + (a × b) == a * b

      , testCase "Vector Jordan Product is Scalar Dot: u ⊙ v == u · v" $ do
          let u = basis @1 * 3 + basis @2 * 4 :: Clifford 2 0 0 Rational
              v = basis @1 * 1 + basis @2 * 2 :: Clifford 2 0 0 Rational
          (u ⊙ v) @?= (u · v)
          (u ⊙ v) @?= scalar 11

      , testCase "Vector Commutator Product is Wedge: u × v == u ∧ v" $ do
          let u = basis @1 * 3 + basis @2 * 4 :: Clifford 2 0 0 Rational
              v = basis @1 * 1 + basis @2 * 2 :: Clifford 2 0 0 Rational
          (u × v) @?= (u ∧ v)
          (u × v) @?= blade 3 2
      ]

  , testGroup "Forced Euclidean Contractions (Metric-Agnostic)"
      [ testCase "Minkowski STA Cl(1,3) spacelike vector squaring: Metric vs Euclidean" $ do
          let gamma1 = basis @2 :: Clifford 1 3 0 Int  -- spacelike vector (gamma1² = -1)
          (gamma1 ⨼ gamma1) @?= scalar (-1)           -- Metric-aware contraction
          leftContractEucl gamma1 gamma1 @?= scalar 1   -- Forced Euclidean ignores negative metric!

      , testCase "PGA Cl(2,0,1) null vector squaring: Metric vs Euclidean" $ do
          let e0 = basis @3 :: Clifford 2 0 1 Int      -- null vector (e0² = 0)
          (e0 ⨼ e0) @?= 0                              -- Metric-aware contraction annihilates
          leftContractEucl e0 e0 @?= scalar 1          -- Forced Euclidean ignores null degeneracy!
          scalarProdEucl e0 e0 @?= 1

      , testProperty "Euclidean Contraction Invariant in Cl(2,0) matches standard Left Contraction" $
          \(a :: Clifford 2 0 0 Int) b -> leftContractEucl a b == (a ⨼ b)
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
            let k = bladeGrade b
            in isEvenSwaps b b == even ((k * (k - 1)) `div` 2)

      , testProperty "Invariant 3 (Complementarity on Disjoint Blades): isEvenSwaps b1 b2 == (even (k1 * k2) == isEvenSwaps b2 b1)" $
          \(b1 :: Blade) (b2Raw :: Blade) ->
            let b2 = b2Raw .&. complement b1  -- ensure b1 and b2 are disjoint (b1 .&. b2 == 0)
                k1 = bladeGrade b1
                k2 = bladeGrade b2
            in isEvenSwaps b1 b2 == (even (k1 * k2) == isEvenSwaps b2 b1)

      , testProperty "Invariant 4 (List Inversion Parity Equivalence): isEvenSwaps matches even of list inversions" $
          \(b1 :: Blade) (b2 :: Blade) ->
            let is = bladeToIndices b1
                js = bladeToIndices b2
                expectedEven = even (length [ () | i <- is, j <- js, i > j ])
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

  , testGroup "Basis Blade Multiplication (multiplyBlades) Invariants"
      [ testProperty "Invariant 1 (Unital Identity): 1 * b == b and b * 1 == b" $
          forAll (choose (0, 31 :: Word)) $ \b ->
            let masks = signatureMasks (Proxy @(Signature 2 3 0))
            in (multiplyBlades masks b 0 == Just (b, True)) &&
               (multiplyBlades masks 0 b == Just (b, True))

      , testCase "Invariant 2 (Basis 1-Vector Squares): e_k² in {+1, -1, 0}" $ do
          let masksCl20 = signatureMasks (Proxy @Cl2_0)
              masksCl02 = signatureMasks (Proxy @Cl0_2)
              masksSTA  = signatureMasks (Proxy @Cl1_3)
              masksPGA  = signatureMasks (Proxy @Cl3_0_1)

          -- Cl(2,0,0) Euclidean: e₁² = +1, e₂² = +1
          multiplyBlades masksCl20 (bit 0) (bit 0) @?= Just (0, True)
          multiplyBlades masksCl20 (bit 1) (bit 1) @?= Just (0, True)
          -- Cl(0,2,0) Quaternions: e₁² = -1, e₂² = -1
          multiplyBlades masksCl02 (bit 0) (bit 0) @?= Just (0, False)
          multiplyBlades masksCl02 (bit 1) (bit 1) @?= Just (0, False)
          -- Cl(1,3,0) STA: e₁² = +1, e₂² = -1, e₃² = -1, e₄² = -1
          multiplyBlades masksSTA (bit 0) (bit 0) @?= Just (0, True)
          multiplyBlades masksSTA (bit 1) (bit 1) @?= Just (0, False)
          multiplyBlades masksSTA (bit 2) (bit 2) @?= Just (0, False)
          multiplyBlades masksSTA (bit 3) (bit 3) @?= Just (0, False)
          -- Cl(3,0,1) PGA: e₁² = +1, e₂² = +1, e₃² = +1, e₀² = 0 (bit 3 is 4th vector)
          multiplyBlades masksPGA (bit 0) (bit 0) @?= Just (0, True)
          multiplyBlades masksPGA (bit 1) (bit 1) @?= Just (0, True)
          multiplyBlades masksPGA (bit 2) (bit 2) @?= Just (0, True)
          multiplyBlades masksPGA (bit 3) (bit 3) @?= Nothing

      , testProperty "Invariant 3 (Orthogonal 1-Vector Anticommutation): e_j * e_k == -(e_k * e_j)" $
          forAll (choose (0, 3 :: Int)) $ \j ->
          forAll (choose (0, 3 :: Int)) $ \k ->
            (j /= k) ==>
              let masks = signatureMasks (Proxy @Cl1_3)
              in case (multiplyBlades masks (bit j) (bit k), multiplyBlades masks (bit k) (bit j)) of
                   (Just (b1, s1), Just (b2, s2)) -> b1 == b2 && s1 /= s2
                   _ -> False

      , testProperty "Invariant 4 (Self-Square is Always a Pure Scalar): b * b is Grade 0 (blade 0) or Nothing" $
          forAll (choose (0, 15 :: Word)) $ \b ->
            let masksSTA = signatureMasks (Proxy @Cl1_3)
                masksPGA = signatureMasks (Proxy @Cl3_0_1)
                checkSTA = case multiplyBlades masksSTA b b of
                             Nothing -> True
                             Just (resB, _) -> resB == 0
                checkPGA = case multiplyBlades masksPGA b b of
                             Nothing -> True
                             Just (resB, _) -> resB == 0
            in checkSTA && checkPGA

      , testProperty "Invariant 5 (Degenerate Subspace Annihilation in PGA): (b1 .&. b2 .&. nullMask) /= 0 ==> product == Nothing" $
          forAll (choose (0, 15 :: Word)) $ \b1 ->
          forAll (choose (0, 15 :: Word)) $ \b2 ->
            let masksPGA = signatureMasks (Proxy @Cl3_0_1)
            in ((b1 .&. b2 .&. nullMask masksPGA) /= 0) ==>
                 (multiplyBlades masksPGA b1 b2 == Nothing)

      , testProperty "Invariant 6 (Grade Additivity on Disjoint Blades): b1 .&. b2 == 0 ==> grade == grade b1 + grade b2" $
          forAll (choose (0, 15 :: Word)) $ \b1 ->
          forAll (choose (0, 15 :: Word)) $ \b2 ->
            (b1 .&. b2 == 0) ==>
              let masksSTA = signatureMasks (Proxy @Cl1_3)
              in case multiplyBlades masksSTA b1 b2 of
                   Just (resB, _) -> bladeGrade resB == bladeGrade b1 + bladeGrade b2
                   Nothing        -> False

      , testProperty "Invariant 7 (Associativity of Basis Blades): (b1 * b2) * b3 == b1 * (b2 * b3)" $
          forAll (choose (0, 7 :: Word)) $ \b1 ->
          forAll (choose (0, 7 :: Word)) $ \b2 ->
          forAll (choose (0, 7 :: Word)) $ \b3 ->
            let lhs = (blade b1 1 * blade b2 1 :: Clifford 2 1 0 Int) * blade b3 1
                rhs = blade b1 1 * (blade b2 1 * blade b3 1 :: Clifford 2 1 0 Int)
            in lhs == rhs

      , testProperty "Invariant 8 (Agreement with Geometric Product): multiplyBlades b1 b2 matches e_I * e_J" $
          forAll (choose (0, 15 :: Word)) $ \b1 ->
          forAll (choose (0, 15 :: Word)) $ \b2 ->
            let masksSTA    = signatureMasks (Proxy @Cl1_3)
                masksPGA    = signatureMasks (Proxy @Cl3_0_1)
                expectedSTA = blade b1 1 * blade b2 1 :: Clifford 1 3 0 Int
                actualSTA   = case multiplyBlades masksSTA b1 b2 of
                                Nothing            -> 0
                                Just (resB, isPos) -> blade resB (if isPos then 1 else -1)
                expectedPGA = blade b1 1 * blade b2 1 :: Clifford 3 0 1 Int
                actualPGA   = case multiplyBlades masksPGA b1 b2 of
                                Nothing            -> 0
                                Just (resB, isPos) -> blade resB (if isPos then 1 else -1)
            in actualSTA == expectedSTA && actualPGA == expectedPGA

      , testProperty "Invariant 9 (General Grade Formula for Arbitrary Blades): grade(b1 * b2) == r + s - 2 * popCount (b1 .&. b2)" $
          forAll (choose (0, 15 :: Word)) $ \b1 ->
          forAll (choose (0, 15 :: Word)) $ \b2 ->
            let masksSTA = signatureMasks (Proxy @Cl1_3)
                r = bladeGrade b1
                s = bladeGrade b2
                k = popCount (b1 .&. b2)
                expectedGrade = r + s - 2 * k
            in case multiplyBlades masksSTA b1 b2 of
                 Nothing -> True
                 Just (resB, _) -> bladeGrade resB == expectedGrade
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

  , testGroup "Scalar Product (scalarProd) & Extractors Invariants"
      [ testProperty "Cyclic Trace Commutativity Cl(2,0): ⟨A * B⟩₀ = ⟨B * A⟩₀" $
          \(a :: Clifford 2 0 0 Int) b ->
            scalarProd a b == scalarProd b a

      , testProperty "Cyclic Trace Commutativity Cl(1,3) STA: ⟨A * B⟩₀ = ⟨B * A⟩₀" $
          \(a :: Clifford 1 3 0 Int) b ->
            scalarProd a b == scalarProd b a

      , testProperty "Cyclic Trace Commutativity Cl(3,0,1) PGA: ⟨A * B⟩₀ = ⟨B * A⟩₀" $
          \(a :: Clifford 3 0 1 Int) b ->
            scalarProd a b == scalarProd b a

      , testProperty "Cyclic Trace on 3 Multivectors: ⟨(A * B) * C⟩₀ = ⟨(B * C) * A⟩₀" $
          \(a :: Clifford 2 0 0 Int) b c ->
            scalarProd (a * b) c == scalarProd (b * c) a &&
            scalarProd (a * b) c == scalarProd (c * a) b

      , testProperty "Bilinear Distributivity: ⟨A * (B + C)⟩₀ = ⟨A * B⟩₀ + ⟨A * C⟩₀" $
          \(a :: Clifford 2 0 0 Int) b c ->
            scalarProd a (b + c) == scalarProd a b + scalarProd a c &&
            scalarProd (a + b) c == scalarProd a c + scalarProd b c

      , testProperty "Grade Orthogonality: ⟨⟨A⟩ᵣ * ⟨B⟩ₛ⟩₀ = 0 when r /= s" $
          \(a :: Clifford 2 0 0 Int) b ->
            and [ scalarProd (grade r a) (grade s b) == 0
                | r <- [0 .. 2]
                , s <- [0 .. 2]
                , r /= s
                ]

      , testProperty "Agreement with scalarCoeff: scalarProd A B == scalarCoeff (A * B)" $
          \(a :: Clifford 2 0 0 Int) b ->
            scalarProd a b == scalarCoeff (a * b)

      , testCase "bladeCoeff and scalarCoeff extraction on inhomogeneous multivector" $ do
          let mv = scalar 5 + basis @1 * 3 - basis @2 * 4 + (basis @1 * basis @2) * 7 :: Clifford 2 0 0 Int
          scalarCoeff mv @?= 5
          bladeCoeff 0 mv @?= 5
          bladeCoeff 1 mv @?= 3
          bladeCoeff 2 mv @?= -4
          bladeCoeff 3 mv @?= 7
          bladeCoeff 4 mv @?= 0 -- Absent blade defaults to 0

      , testProperty "Positive-Definiteness of cliffordScalar: ⟨A * ~A⟩₀ > 0 for all A /= 0 in Cl(2,0)" $
          \(a :: Clifford 2 0 0 Int) ->
            let s = cliffordScalar a a
            in (a == 0 && s == 0) || (a /= 0 && s > 0)

      , testProperty "Positive-Definiteness of cliffordScalar: ⟨A * ~A⟩₀ > 0 for all A /= 0 in Cl(3,0)" $
          \(a :: Clifford 3 0 0 Int) ->
            let s = cliffordScalar a a
            in (a == 0 && s == 0) || (a /= 0 && s > 0)

      , testProperty "Symmetry of cliffordScalar: ⟨A * ~B⟩₀ = ⟨B * ~A⟩₀ across Cl(2,0)" $
          \(a :: Clifford 2 0 0 Int) b ->
            cliffordScalar a b == cliffordScalar b a

      , testProperty "Symmetry of cliffordScalar: ⟨A * ~B⟩₀ = ⟨B * ~A⟩₀ across Cl(1,3) STA" $
          \(a :: Clifford 1 3 0 Int) b ->
            cliffordScalar a b == cliffordScalar b a

      , testProperty "Symmetry of cliffordScalar: ⟨A * ~B⟩₀ = ⟨B * ~A⟩₀ across Cl(3,0,1) PGA" $
          \(a :: Clifford 3 0 1 Int) b ->
            cliffordScalar a b == cliffordScalar b a

      , testProperty "Bilinearity of cliffordScalar: ⟨A * ~(B + C)⟩₀ = ⟨A * ~B⟩₀ + ⟨A * ~C⟩₀" $
          \(a :: Clifford 2 0 0 Int) b c ->
            cliffordScalar a (b + c) == cliffordScalar a b + cliffordScalar a c &&
            cliffordScalar (a + b) c == cliffordScalar a c + cliffordScalar b c

      , testProperty "Reversion Invariance of cliffordScalar: ⟨~A * ~~B⟩₀ = ⟨A * ~B⟩₀" $
          \(a :: Clifford 2 0 0 Int) b ->
            cliffordScalar (reverseCl a) (reverseCl b) == cliffordScalar a b

      , testProperty "Orthonormality of Basis Blades in Euclidean Cl(2,0)" $
          forAll (choose (0, 3 :: Word)) $ \b1 ->
          forAll (choose (0, 3 :: Word)) $ \b2 ->
            let blade1 = blade b1 1 :: Clifford 2 0 0 Int
                blade2 = blade b2 1 :: Clifford 2 0 0 Int
                expected = if b1 == b2 then 1 else 0
            in cliffordScalar blade1 blade2 == expected

      , testCase "Euclidean Bivector Square: scalarProd e12 e12 == -1 vs cliffordScalar e12 e12 == 1" $ do
          let e12 = basis @1 * basis @2 :: Clifford 2 0 0 Int
          scalarProd e12 e12 @?= -1
          cliffordScalar e12 e12 @?= 1
      ]
  ]
