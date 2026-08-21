{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lambda.MultiVectorTest (multiVectorTests) where

import Lambda.MultiVector
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

-- | QuickCheck wrapper for pure 1-vectors (s=0, b=0)
newtype PureVector = PV { unPV :: Multivector }
  deriving (Show, Eq)

instance Arbitrary PureVector where
  arbitrary = PV <$> (vec <$> choose (-50, 50) <*> choose (-50, 50))

-- | Arbitrary generator for random multivectors including pure grades and zero
instance Arbitrary Multivector where
  arbitrary =
    oneof
      [ -- General multivector
        MV <$> choose (-50, 50) <*> choose (-50, 50) <*> choose (-50, 50) <*> choose (-50, 50),
        -- Pure bivector (s=0, x=0, y=0)
        biv <$> choose (-50, 50),
        -- Pure vector (s=0, b=0) via PureVector generator
        unPV <$> arbitrary,
        -- Pure scalar (x=0, y=0, b=0)
        toScalar <$> choose (-50, 50),
        -- Exact zero
        pure 0
      ]

-- | Approximate equality helper for floating-point comparisons
approxEq :: Multivector -> Multivector -> Bool
approxEq (MV s1 x1 y1 b1) (MV s2 x2 y2 b2) =
  let eps = 1e-6
   in abs (s1 - s2) < eps
        && abs (x1 - x2) < eps
        && abs (y1 - y2) < eps
        && abs (b1 - b2) < eps

multiVectorTests :: TestTree
multiVectorTests =
  testGroup
    "MultiVector Geometric Algebra Tests"
    [ testGroup
        "Num Typeclass Laws"
        [ testProperty "Additive Associativity: (a + b) + c == a + (b + c)" $
            \a b c -> approxEq ((a + b) + c) (a + (b + c)),
          testProperty "Additive Commutativity: a + b == b + a" $
            \(a :: Multivector) b -> (a + b) == (b + a),
          testProperty "Additive Identity: a + 0 == a and 0 + a == a" $
            \(a :: Multivector) -> (a + 0) == a && (0 + a) == a,
          testProperty "Additive Inverse: a - a == 0 and a + negate a == 0" $
            \(a :: Multivector) -> (a - a) == 0 && (a + negate a) == 0,
          testProperty "Multiplicative Associativity: (a * b) * c == a * (b * c)" $
            \a b c -> approxEq ((a * b) * c) (a * (b * c)),
          testProperty "Multiplicative Identity: a * 1 == a and 1 * a == a" $
            \(a :: Multivector) -> (a * 1) == a && (1 * a) == a,
          testProperty "Left Distributivity: a * (b + c) == (a * b) + (a * c)" $
            \a b c -> approxEq (a * (b + c)) ((a * b) + (a * c)),
          testProperty "Right Distributivity: (a + b) * c == (a * c) + (b * c)" $
            \a b c -> approxEq ((a + b) * c) ((a * c) + (b * c)),
          testProperty "Magnitude & Direction Law: abs a * signum a == a" $
            \a -> approxEq (abs a * signum a) a,
          testProperty "Abs Idempotence: abs (abs a) == abs a" $
            \a -> approxEq (abs (abs a)) (abs a)
        ],
      testGroup
        "Geometric Algebra Laws & Vector Product"
        [ testProperty "Vector Product: u * v == (u · v) + (u ∧ v)" $
            \(PV u) (PV v) -> (u * v) == (u · v + u ∧ v),
          testProperty "Wedge Antisymmetry on 1-vectors: u ∧ v == -(v ∧ u)" $
            \(PV u) (PV v) -> (u ∧ v) == negate (v ∧ u),
          testProperty "Wedge Nilpotency on 1-vectors: u ∧ u == 0" $
            \(PV u) -> (u ∧ u) == 0,
          testProperty "Dot Symmetry on 1-vectors: u · v == v · u" $
            \(PV u) (PV v) -> (u · v) == (v · u),
          testProperty "Wedge is Grade-2 projection on 1-vectors: u ∧ v == grade 2 (u * v)" $
            \(PV u) (PV v) -> (u ∧ v) == grade 2 (u * v),
          testProperty "Wedge Associativity: (a ∧ b) ∧ c == a ∧ (b ∧ c)" $
            \a b c -> approxEq ((a ∧ b) ∧ c) (a ∧ (b ∧ c))
        ],
      testGroup
        "Basis Concrete Values"
        [ testCase "e1 * e1 = 1" $
            e1v * e1v @?= MV 1 0 0 0,
          testCase "e2 * e2 = 1" $
            e2v * e2v @?= MV 1 0 0 0,
          testCase "e1 * e2 = e12" $
            e1v * e2v @?= e12v,
          testCase "e2 * e1 = -e12" $
            e2v * e1v @?= MV 0 0 0 (-1),
          testCase "e12 * e12 = -1" $
            e12v * e12v @?= MV (-1) 0 0 0,
          testCase "(e1 ^ e2) . (e1 ^ e2) + (e1 ^ e2) ^ (e1 ^ e2) = -1" $
            let b12 = wedge e1v e2v
                res = scalarProd b12 b12 + wedge b12 b12
             in res @?= MV (-1) 0 0 0,
          testCase "abs e12v = 1 (pure bivector magnitude)" $
            abs e12v @?= toScalar 1,
          testCase "abs (vec 3 4) = 5 (vector magnitude)" $
            abs (vec 3 4) @?= toScalar 5,
          testCase "e12v ∗ e12v = 1 (positive-definite bivector square)" $
            (e12v ∗ e12v) @?= toScalar 1,
          testCase "e12v · e12v = -1 (indefinite scalar product)" $
            (e12v · e12v) @?= toScalar (-1)
        ],
      testGroup
        "Rotor & Sandwich Rotation Tests"
        [ testCase "rotateVector e1v (pi / 2) == e2v (90 deg counter-clockwise)" $
            assertBool "e1 rotated 90 deg is e2" $
              approxEq (rotateVector e1v (pi / 2)) e2v,
          testCase "rotateVector e2v (pi / 2) == -e1v (90 deg counter-clockwise)" $
            assertBool "e2 rotated 90 deg is -e1" $
              approxEq (rotateVector e2v (pi / 2)) (negate e1v),
          testCase "rotateVector e1v pi == -e1v (180 deg)" $
            assertBool "e1 rotated 180 deg is -e1" $
              approxEq (rotateVector e1v pi) (negate e1v),
          testProperty "Rotation preserves length (Isometry): abs (rotateVector v theta) == abs v" $
            \(PV v) theta ->
              approxEq (abs (rotateVector v theta)) (abs v),
          testProperty "Rotation Identity: rotateVector v 0 == v" $
            \(PV v) ->
              rotateVector v 0 == v,
          testProperty "Rotation Inverse: rotateVector (rotateVector v theta) (-theta) == v" $
            \(PV v) theta ->
              approxEq (rotateVector (rotateVector v theta) (-theta)) v,
          testProperty "Rotation 360 deg (2*pi) is Identity: rotateVector v (2 * pi) == v" $
            \(PV v) ->
              approxEq (rotateVector v (2 * pi)) v,
          testProperty "Rotation Angle Additivity / Composition: rotateVector (rotateVector v t1) t2 == rotateVector v (t1 + t2)" $
            \(PV v) t1 t2 ->
              approxEq (rotateVector (rotateVector v t1) t2) (rotateVector v (t1 + t2)),
          testProperty "Dot Product Invariance: (rotateVector u theta · rotateVector v theta) == u · v" $
            \(PV u) (PV v) theta ->
              approxEq (rotateVector u theta · rotateVector v theta) (u · v),
          testProperty "Wedge Product / Area Invariance: (rotateVector u theta ∧ rotateVector v theta) == u ∧ v" $
            \(PV u) (PV v) theta ->
              approxEq (rotateVector u theta ∧ rotateVector v theta) (u ∧ v),
          testProperty "Linearity over Vector Addition: rotateVector (u + v) theta == rotateVector u theta + rotateVector v theta" $
            \(PV u) (PV v) theta ->
              approxEq (rotateVector (u + v) theta) (rotateVector u theta + rotateVector v theta),
          testProperty "Linearity over Scalar Scaling: rotateVector (toScalar c * v) theta == toScalar c * rotateVector v theta" $
            \c (PV v) theta ->
              approxEq (rotateVector (toScalar c * v) theta) (toScalar c * rotateVector v theta),
          testProperty "90 deg CCW rotation matches duality: rotateVector v (pi / 2) == v * e12v" $
            \(PV v) ->
              approxEq (rotateVector v (pi / 2)) (v * e12v),
          testProperty "90 deg CCW duality coordinate transform: v * e12v == vec (- e2 v) (e1 v)" $
            \(PV v) ->
              (v * e12v) == vec (- e2 v) (e1 v),
          testProperty "90 deg CW rotation matches duality: rotateVector v (-pi / 2) == e12v * v" $
            \(PV v) ->
              approxEq (rotateVector v (-pi / 2)) (e12v * v),
          testProperty "90 deg CW duality coordinate transform: e12v * v == vec (e2 v) (- e1 v)" $
            \(PV v) ->
              (e12v * v) == vec (e2 v) (- e1 v),
          testProperty "Vector and Bivector anti-commute: e12v * v == negate (v * e12v)" $
            \(PV v) ->
              (e12v * v) == negate (v * e12v)
        ],
      testGroup
        "Reversion & Involution Laws"
        [ testProperty "Involution: reverseMV (reverseMV a) == a" $
            \a ->
              reverseMV (reverseMV a) == a,
          testProperty "Product Order Reversal: reverseMV (a * b) == reverseMV b * reverseMV a" $
            \a b ->
              approxEq (reverseMV (a * b)) (reverseMV b * reverseMV a),
          testProperty "Wedge Order Reversal: reverseMV (a ∧ b) == reverseMV b ∧ reverseMV a" $
            \a b ->
              approxEq (reverseMV (a ∧ b)) (reverseMV b ∧ reverseMV a),
          testProperty "1-Vectors are Self-Reverse: reverseMV v == v" $
            \(PV v) ->
              reverseMV v == v,
          testProperty "Reversion Linearity: reverseMV (a + b) == reverseMV a + reverseMV b" $
            \a b ->
              (reverseMV (a + b)) == (reverseMV a + reverseMV b)
        ],
      testGroup
        "Clifford Scalar Product (Positive-Definite) Laws"
        [ testProperty "Positive-Definiteness: scalar (a ∗ a) >= 0" $
            \a ->
              scalar (a ∗ a) >= 0,
          testProperty "Non-degeneracy: (a ∗ a == 0) == (a == 0)" $
            \a ->
              (a ∗ a == 0) == (a == 0),
          testProperty "Symmetry: (a ∗ b) == (b ∗ a)" $
            \a b ->
              (a ∗ b) == (b ∗ a),
          testProperty "Agreement on 1-vectors with Dot Product: (u ∗ v) == (u · v)" $
            \(PV u) (PV v) ->
              (u ∗ v) == (u · v),
          testProperty "Coordinate Formula: (MV sA xA yA bA ∗ MV sB xB yB bB)" $
            \sA xA yA bA sB xB yB bB ->
              (MV sA xA yA bA ∗ MV sB xB yB bB) == toScalar (sA * sB + xA * xB + yA * yB + bA * bB),
          testProperty "Relation to Magnitude (abs): abs a * abs a ≈ a ∗ a" $
            \a ->
              approxEq (abs a * abs a) (a ∗ a),
          testProperty "Bilinear Distributivity: a ∗ (b + c) ≈ (a ∗ b) + (a ∗ c)" $
            \a b c ->
              approxEq (a ∗ (b + c)) ((a ∗ b) + (a ∗ c))
        ],
      testGroup
        "Hestenes Inner Product (•) Laws"
        [ testCase "e1 • e12 = e2 (in-plane orthogonal complement)" $
            (e1v • e12v) @?= e2v,
          testCase "e2 • e12 = -e1 (in-plane orthogonal complement)" $
            (e2v • e12v) @?= negate e1v,
          testCase "e12 • e12 = -1 (bivector contraction)" $
            (e12v • e12v) @?= toScalar (-1),
          testProperty "Agreement on 1-vectors with Dot Product: u • v == u · v" $
            \(PV u) (PV v) ->
              (u • v) == (u · v),
          testProperty "Symmetry on 1-vectors: u • v == v • u" $
            \(PV u) (PV v) ->
              (u • v) == (v • u),
          testProperty "Anti-symmetry with Bivector: v • (biv b) == negate ((biv b) • v)" $
            \(PV v) b ->
              (v • biv b) == negate (biv b • v),
          testProperty "Symmetry on Bivectors: (biv b1) • (biv b2) == (biv b2) • (biv b1)" $
            \b1 b2 ->
              (biv b1 • biv b2) == (biv b2 • biv b1),
          testProperty "Scalars Drop Out (Nilpotency on Grade 0): toScalar s • a == 0" $
            \s a ->
              (toScalar s • a) == 0 && (a • toScalar s) == 0,
          testProperty "Bilinear Distributivity: a • (b + c) == (a • b) + (a • c)" $
            \a b c ->
              approxEq (a • (b + c)) ((a • b) + (a • c))
        ],
      testGroup
        "Fat Dot Product (●) Laws"
        [ testCase "3 ● e1 = 3 * e1 (scalar scales vector)" $
            (toScalar 3 ● e1v) @?= vec 3 0,
          testCase "3 ● e12 = 3 * e12 (scalar scales bivector)" $
            (toScalar 3 ● e12v) @?= biv 3,
          testCase "e1 ● e12 = e2 (vector-bivector contraction)" $
            (e1v ● e12v) @?= e2v,
          testProperty "Agreement on 1-vectors with Dot Product: u ● v == u · v" $
            \(PV u) (PV v) ->
              (u ● v) == (u · v),
          testProperty "Scalars Scale Arbitrary Multivectors: toScalar s ● a == toScalar s * a" $
            \s a ->
              approxEq (toScalar s ● a) (toScalar s * a),
          testProperty "Scalar Right Scaling: a ● toScalar s == toScalar s * a" $
            \s a ->
              approxEq (a ● toScalar s) (toScalar s * a),
          testProperty "Agrees with Hestenes on Non-Zero Grades" $
            \xa ya ba xb yb bb ->
              let a = MV 0 xa ya ba
                  b = MV 0 xb yb bb
               in approxEq (a ● b) (a • b),
          testProperty "Bilinear Distributivity: a ● (b + c) == (a ● b) + (a ● c)" $
            \a b c ->
              approxEq (a ● (b + c)) ((a ● b) + (a ● c))
        ],
      testGroup
        "Left Contraction (⨼) Laws"
        [ testCase "3 ⨼ e1 = 3 * e1 (scalar scales vector)" $
            (toScalar 3 ⨼ e1v) @?= vec 3 0,
          testCase "e1 ⨼ e12 = e2 (extracts e1 from plane)" $
            (e1v ⨼ e12v) @?= e2v,
          testCase "e2 ⨼ e12 = -e1 (extracts e2 from plane)" $
            (e2v ⨼ e12v) @?= negate e1v,
          testCase "e12 ⨼ e1 = 0 (cannot contract higher grade 2 out of lower grade 1)" $
            (e12v ⨼ e1v) @?= MV 0 0 0 0,
          testCase "e12 ⨼ e12 = -1 (bivector contraction)" $
            (e12v ⨼ e12v) @?= toScalar (-1),
          testProperty "Agreement on 1-vectors with Dot Product: u ⨼ v == u · v" $
            \(PV u) (PV v) ->
              (u ⨼ v) == (u · v),
          testProperty "Grade annihilation when r > s: (biv b) ⨼ v == 0" $
            \b (PV v) ->
              (biv b ⨼ v) == 0,
          testProperty "Contraction Adjoint Duality to Wedge: <(a ∧ b) * c>₀ == <a * (b ⨼ c)>₀" $
            \a b c ->
              approxEq (grade 0 ((a ∧ b) * c)) (grade 0 (a * (b ⨼ c))),
          testProperty "Bilinear Distributivity: a ⨼ (b + c) == (a ⨼ b) + (a ⨼ c)" $
            \a b c ->
              approxEq (a ⨼ (b + c)) ((a ⨼ b) + (a ⨼ c))
        ]
    ]
