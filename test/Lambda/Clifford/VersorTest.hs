{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lambda.Clifford.VersorTest (versorTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.Ratio (Ratio, (%))

import Lambda.Clifford.Signature
import Lambda.Clifford.Universal
import Lambda.Clifford.Versor

versorTests :: TestTree
versorTests = testGroup "Clifford Versor, Rotor & Transformation Tests"
  [ testGroup "Smart Constructors & Validation"
      [ testCase "fromVector accepts pure non-zero 1-vectors" $ do
          let e1 = basis @1 :: Clifford 2 0 0 (Ratio Integer)
              e2 = basis @2 :: Clifford 2 0 0 (Ratio Integer)
          case fromVector e1 of
            Nothing -> assertFailure "Expected Just Pinor for basis vector e1"
            Just p  -> unVersor p @?= e1
          case fromVector (e1 * 3 + e2 * 4) of
            Nothing -> assertFailure "Expected Just Pinor for 3·e₁ + 4·e₂"
            Just p  -> unVersor p @?= (e1 * 3 + e2 * 4)

      , testCase "fromVector rejects scalars, bivectors, and mixed multivectors" $ do
          let e1 = basis @1 :: Clifford 2 0 0 (Ratio Integer)
              e2 = basis @2 :: Clifford 2 0 0 (Ratio Integer)
              s  = scalar 5 :: Clifford 2 0 0 (Ratio Integer)
              b  = e1 * e2
              m  = e1 + b
          fromVector s @?= Nothing
          fromVector b @?= Nothing
          fromVector m @?= Nothing

      , testCase "fromVectorPair produces an even Rotor" $ do
          let e1 = basis @1 :: Clifford 2 0 0 (Ratio Integer)
              e2 = basis @2 :: Clifford 2 0 0 (Ratio Integer)
          case fromVectorPair e1 e2 of
            Nothing -> assertFailure "Expected Just Rotor for pair (e1, e2)"
            Just r  -> unVersor r @?= (e1 * e2)

      , testCase "fromVectorProduct produces a Versor of Any parity" $ do
          let e1 = basis @1 :: Clifford 3 0 0 (Ratio Integer)
              e2 = basis @2 :: Clifford 3 0 0 (Ratio Integer)
              e3 = basis @3 :: Clifford 3 0 0 (Ratio Integer)
          case fromVectorProduct [e1, e2, e3] of
            Nothing -> assertFailure "Expected Just Versor for 3-vector product"
            Just v  -> unVersor v @?= (e1 * e2 * e3)
      ]

  , testGroup "Outermorphism Validation & isVersor"
      [ testCase "isVersor detects true versors and rejects non-versors" $ do
          let e1 = basis @1 :: Clifford 2 0 0 (Ratio Integer)
              e2 = basis @2 :: Clifford 2 0 0 (Ratio Integer)
          -- Valid versors
          isVersor e1 @?= True
          isVersor (e1 * e2) @?= True
          isVersor (scalar 1 + e1 * e2) @?= True
          -- Invalid mixed multivector
          isVersor (e1 + e1 * e2) @?= False
          -- Zero multivector (uninvertible)
          isVersor (zeroClifford :: Clifford 2 0 0 (Ratio Integer)) @?= False

      , testCase "mkRotor and mkPinor dynamic classification" $ do
          let e1 = basis @1 :: Clifford 2 0 0 (Ratio Integer)
              e2 = basis @2 :: Clifford 2 0 0 (Ratio Integer)
          case mkRotor (e1 * e2) of
            Nothing -> assertFailure "Expected Just Rotor for e1*e2"
            Just _  -> pure ()
          mkRotor e1 @?= Nothing
          case mkPinor e1 of
            Nothing -> assertFailure "Expected Just Pinor for e1"
            Just _  -> pure ()
          mkPinor (e1 * e2) @?= Nothing
      ]

  , testGroup "Compile-Time Parity Composition (⊗)"
      [ testCase "Even ⊗ Even = Even (Rotor composition)" $ do
          let e1 = basis @1 :: Clifford 2 0 0 (Ratio Integer)
              e2 = basis @2 :: Clifford 2 0 0 (Ratio Integer)
          case (fromVectorPair e1 e2, fromVectorPair e2 e1) of
            (Just r1, Just r2) -> do
              let rTotal = r1 ⊗ r2
              unVersor rTotal @?= (e1 * e2 * e2 * e1)
              unVersor rTotal @?= scalar 1
            _ -> assertFailure "Failed to construct test rotors"

      , testCase "Odd ⊗ Odd = Even (Two reflections compose to a rotation!)" $ do
          let e1 = basis @1 :: Clifford 2 0 0 (Ratio Integer)
              e2 = basis @2 :: Clifford 2 0 0 (Ratio Integer)
          case (fromVector e1, fromVector e2) of
            (Just p1, Just p2) -> do
              let rTotal = p1 ⊗ p2  -- Type inference automatically verifies this is a Rotor ('Even)!
              unVersor rTotal @?= (e1 * e2)
            _ -> assertFailure "Failed to construct test pinors"

      , testCase "Even ⊗ Odd = Odd (Rotation and reflection compose to a reflection)" $ do
          let e1 = basis @1 :: Clifford 2 0 0 (Ratio Integer)
              e2 = basis @2 :: Clifford 2 0 0 (Ratio Integer)
          case (fromVectorPair e1 e2, fromVector e1) of
            (Just r, Just p) -> do
              let pTotal = r ⊗ p  -- Type inference verifies this is a Pinor ('Odd)!
              unVersor pTotal @?= (e1 * e2 * e1)
              unVersor pTotal @?= negate e2
            _ -> assertFailure "Failed to construct test elements"
      ]

  , testGroup "Geometric Transformations (Sandwich Products)"
      [ testCase "Single Vector Reflection: x' = - n · x · n⁻¹" $ do
          let e1 = basis @1 :: Clifford 2 0 0 (Ratio Integer)
              e2 = basis @2 :: Clifford 2 0 0 (Ratio Integer)
          case reflectionFromUnitVector e2 of
            Nothing -> assertFailure "Failed to create unit reflection"
            Just p2 -> do
              -- Reflecting e1 across hyperplane orthogonal to e2 (the x-axis) leaves e1 unchanged:
              applyPinor p2 e1 @?= e1
              -- Reflecting e2 across itself flips e2 to -e2:
              applyPinor p2 e2 @?= negate e2

      , testCase "90° Planar Rotation via rotorFromPlaneAngle" $ do
          let e1 = basis @1 :: Clifford 2 0 0 Double
              e2 = basis @2 :: Clifford 2 0 0 Double
              planeE12 = 3 :: Blade -- 0b0011 (e₁₂)
              piHalf = pi / 2
          case rotorFromPlaneAngle @2 @0 @0 planeE12 piHalf of
            Nothing -> assertFailure "Failed to construct 90° rotor"
            Just r90 -> do
              let e1Rot = applyRotor r90 e1
                  e2Rot = applyRotor r90 e2
              -- Rotating e1 by 90° gives e2:
              abs (bladeCoeff 1 e1Rot) < 1e-10 @? "e1 component near 0"
              abs (bladeCoeff 2 e1Rot - 1.0) < 1e-10 @? "e2 component near 1"
              -- Rotating e2 by 90° gives -e1:
              abs (bladeCoeff 1 e2Rot - (-1.0)) < 1e-10 @? "e1 component near -1"
              abs (bladeCoeff 2 e2Rot) < 1e-10 @? "e2 component near 0"

      , testCase "rotorFromTwoVectors rotating e1 to e2" $ do
          let e1 = basis @1 :: Clifford 2 0 0 Double
              e2 = basis @2 :: Clifford 2 0 0 Double
          case rotorFromTwoVectors e1 e2 of
            Nothing -> assertFailure "Failed to construct rotor from two vectors"
            Just r -> do
              let transformed = applyRotor r e1
              abs (bladeCoeff 1 transformed) < 1e-10 @? "e1 component near 0"
              abs (bladeCoeff 2 transformed - 1.0) < 1e-10 @? "e2 component near 1"

      , testCase "Two reflections equal one rotor rotation on arbitrary vectors" $ do
          let e1 = basis @1 :: Clifford 2 0 0 (Ratio Integer)
              e2 = basis @2 :: Clifford 2 0 0 (Ratio Integer)
              testVec = e1 * 3 + e2 * 7
          case (fromVector e1, fromVector e2) of
            (Just p1, Just p2) -> do
              let r = p1 ⊗ p2
                  twoReflections = applyPinor p1 (applyPinor p2 testVec)
                  oneRotation = applyRotor r testVec
              twoReflections @?= oneRotation
            _ -> assertFailure "Failed to construct pinors"
      ]

  , testGroup "Inversion & Group Laws"
      [ testCase "invertRotor is zero-cost reversion: R * ~R = 1" $ do
          let e1 = basis @1 :: Clifford 2 0 0 Double
              e2 = basis @2 :: Clifford 2 0 0 Double
              planeE12 = 3 :: Blade
          case rotorFromPlaneAngle @2 @0 @0 @Double planeE12 (pi / 3) of
            Nothing -> assertFailure "Failed to build rotor"
            Just r -> do
              let rInv = invertRotor r
                  prod = unVersor r * unVersor rInv
              abs (scalarCoeff prod - 1.0) < 1e-10 @? "R * R⁻¹ == 1"

      , testCase "Rotor Monoid Left & Right Identity" $ do
          let e1 = basis @1 :: Clifford 2 0 0 (Ratio Integer)
              e2 = basis @2 :: Clifford 2 0 0 (Ratio Integer)
          case fromVectorPair e1 e2 of
            Nothing -> assertFailure "Failed to build rotor"
            Just r -> do
              (mempty <> r) @?= r
              (r <> mempty) @?= r
      ]

  , testGroup "Quaternion / 3D Even Multivector Factorization Properties"
      [ testProperty "Any non-zero Cl⁺(3, 0, 0) factors into two 1-vectors (u, v) with u * v == q" $
          forAll (arbitrary :: Gen (Double, Double, Double, Double)) $ \(s, b23, b31, b12) ->
            let normSq = s*s + b23*b23 + b31*b31 + b12*b12
            in normSq > 1e-4 ==>
              let e1 = basisIndex @3 @0 @0 1 :: Clifford 3 0 0 Double
                  e2 = basisIndex @3 @0 @0 2 :: Clifford 3 0 0 Double
                  e3 = basisIndex @3 @0 @0 3 :: Clifford 3 0 0 Double
                  q = scalar s + scalar b23 * (e2 * e3) + scalar b31 * (e3 * e1) + scalar b12 * (e1 * e2)
              in case factorEvenCl300 q of
                   Nothing -> False
                   Just (u, v) ->
                     let isVecU = grade 1 u == u
                         isVecV = grade 1 v == v
                         diff = (u * v) - q
                         err = scalarCoeff (diff * reverseCl diff)
                     in isVecU && isVecV && err < 1e-8 && isVersor q

      , testProperty "Any non-zero Cl⁺(0, 3, 0) factors into two 1-vectors (u, v) with u * v == q" $
          forAll (arbitrary :: Gen (Double, Double, Double, Double)) $ \(s, b23, b31, b12) ->
            let normSq = s*s + b23*b23 + b31*b31 + b12*b12
            in normSq > 1e-4 ==>
              let e1 = basisIndex @0 @3 @0 1 :: Clifford 0 3 0 Double
                  e2 = basisIndex @0 @3 @0 2 :: Clifford 0 3 0 Double
                  e3 = basisIndex @0 @3 @0 3 :: Clifford 0 3 0 Double
                  q = scalar s + scalar b23 * (e2 * e3) + scalar b31 * (e3 * e1) + scalar b12 * (e1 * e2)
              in case factorEvenCl030 q of
                   Nothing -> False
                   Just (u, v) ->
                     let isVecU = grade 1 u == u
                         isVecV = grade 1 v == v
                         diff = (u * v) - q
                         err = scalarCoeff (diff * reverseCl diff)
                     in isVecU && isVecV && err < 1e-8 && isVersor q
      ]
  ]
