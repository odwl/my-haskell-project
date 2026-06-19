{-# OPTIONS_GHC -Wno-orphans #-}
module Lambda.LimitTest (limitTests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Lambda.Limit

instance Arbitrary (TerminalM a) where
  arbitrary = pure (TerminalM ())

instance Arbitrary a => Arbitrary (EqualizerOb a) where
  arbitrary = EqualizerOb <$> arbitrary


prop_functor_identity :: TerminalM Int -> Property
prop_functor_identity x = fmap id x === x

prop_functor_composition :: Fun Int String -> Fun String Double -> TerminalM Int -> Property
prop_functor_composition (Fun _ f) (Fun _ g) x =
  fmap (g . f) x === fmap g (fmap f x)

prop_equalizer_properties :: Int -> Fun Int Int -> Fun Int Int -> Property
prop_equalizer_properties x (Fun _ f) (Fun _ tempG) =
  let g y = if y == x then f x else tempG y
      pair = DoubleArrow f g
      h :: () -> Int
      h () = x
  in case equalize pair of
       Equalizer inc fact ->
         case fact h of
           Nothing -> property False
           Just u  ->
             let val = u ()
             in (f (inc val) === g (inc val))
             .&&.
             (inc val === h ())

prop_functor_identity_law :: Int -> Property
prop_functor_identity_law val =
  let eqVal = EqualizerOb val
      pair = DoubleArrow id id
  in mapEqualizer (idCommutingSquare pair) eqVal === eqVal

prop_functor_composition_law :: Int -> Fun Int Int -> Fun Int Int -> Fun Int Int -> Fun Int Int -> Property
prop_functor_composition_law val (Fun _ alpha1) (Fun _ beta1) (Fun _ alpha2) (Fun _ beta2) =
  let eqVal = EqualizerOb val
      s1 = DoubleArrow id id
      s2 = DoubleArrow id id
      s3 = DoubleArrow id id
      hom1 = CommutingSquare s1 s2 alpha1 beta1
      hom2 = CommutingSquare s2 s3 alpha2 beta2
      homComp = compCommutingSquare hom2 hom1
  in mapEqualizer homComp eqVal === mapEqualizer hom2 (mapEqualizer hom1 eqVal)

prop_functor_correctness_law 
  :: Int 
  -> Fun Int Int
  -> Fun Int Int
  -> Fun Int Int
  -> Property
prop_functor_correctness_law x (Fun _ f1) (Fun _ tempG1) (Fun _ betaVal) =
  let g1 v = if v == x then f1 x else tempG1 v
      alphaVal v = v + 1
      alpha_inv v = v - 1
      f2 v = betaVal (f1 (alpha_inv v))
      g2 v = betaVal (g1 (alpha_inv v))
      s1 = DoubleArrow f1 g1
      s2 = DoubleArrow f2 g2
      eqVal1 = EqualizerOb x
      hom = CommutingSquare s1 s2 alphaVal betaVal
      eqVal2 = mapEqualizer hom eqVal1
      yRes = runEqualizerOb eqVal2
  in (yRes === alphaVal x)
     .&&.
     (f2 yRes === g2 yRes)





prop_commuting_square_laws
  :: Int
  -> Fun Int Int
  -> Fun Int Int
  -> Fun Int Int
  -> Property
prop_commuting_square_laws x (Fun _ f1) (Fun _ tempG1) (Fun _ betaVal) =
  let g1 v = if v == x then f1 x else tempG1 v
      alphaVal v = v + 1
      alpha_inv v = v - 1
      f2 v = betaVal (f1 (alpha_inv v))
      g2 v = betaVal (g1 (alpha_inv v))
      s1 = DoubleArrow f1 g1
      s2 = DoubleArrow f2 g2
      hom = CommutingSquare s1 s2 alphaVal betaVal
  in (f2 (alpha hom x) === beta hom (f1 x))
     .&&.
     (g2 (alpha hom x) === beta hom (g1 x))

prop_commuting_square_composition_laws
  :: Int
  -> Fun Int Int
  -> Fun Int Int
  -> Fun Int Int
  -> Fun Int Int
  -> Property
prop_commuting_square_composition_laws x (Fun _ f1) (Fun _ tempG1) (Fun _ beta1) (Fun _ beta2) =
  let g1 y = if y == x then f1 x else tempG1 y
      alpha1 y = y + 1
      alpha1_inv y = y - 1
      f2 y = beta1 (f1 (alpha1_inv y))
      g2 y = beta1 (g1 (alpha1_inv y))
      s1 = DoubleArrow f1 g1
      s2 = DoubleArrow f2 g2
      hom1 = CommutingSquare s1 s2 alpha1 beta1

      alpha2 y = y * 2
      alpha2_inv y = y `div` 2
      f3 y = beta2 (f2 (alpha2_inv y))
      g3 y = beta2 (g2 (alpha2_inv y))
      s3 = DoubleArrow f3 g3
      hom2 = CommutingSquare s2 s3 alpha2 beta2

      homComp = compCommutingSquare hom2 hom1
      alpha_comp = alpha homComp
      beta_comp = beta homComp
  in (f3 (alpha_comp x) === beta_comp (f1 x))
     .&&.
     (g3 (alpha_comp x) === beta_comp (g1 x))


prop_eq_monad_left_identity :: Int -> Fun Int (EqualizerOb String) -> Property
prop_eq_monad_left_identity x (Fun _ f) =
  (return x >>= f) === f x

prop_eq_monad_right_identity :: EqualizerOb Int -> Property
prop_eq_monad_right_identity m =
  (m >>= return) === m

prop_eq_monad_associativity :: EqualizerOb Int -> Fun Int (EqualizerOb String) -> Fun String (EqualizerOb Double) -> Property
prop_eq_monad_associativity m (Fun _ f) (Fun _ g) =
  ((m >>= f) >>= g) === (m >>= (\x -> f x >>= g))

prop_mapEqualizerM_composition 
  :: Fun Int Int -> Fun Int Int
  -> Fun Int Int -> Fun Int Int
  -> Property
prop_mapEqualizerM_composition (Fun _ alpha1) (Fun _ beta1) (Fun _ alpha2) (Fun _ beta2) =
  let s1 = DoubleArrow id id
      s2 = DoubleArrow id id
      s3 = DoubleArrow id id
      hom1 = CommutingSquare s1 s2 alpha1 beta1
      hom2 = CommutingSquare s2 s3 alpha2 beta2
      homComp = compCommutingSquare hom2 hom1
      lhs = mapEqualizerM homComp
      rhs = compCommutingSquare (mapEqualizerM hom2) (mapEqualizerM hom1)
  in (alpha lhs (EqualizerOb 42) === alpha rhs (EqualizerOb 42))
     .&&.
     (beta lhs (EqualizerOb 42) === beta rhs (EqualizerOb 42))


prop_monad_natural_left_unit :: Int -> Property
prop_monad_natural_left_unit val =
  let pair = DoubleArrow id id
      tPair = DoubleArrow (fmap id) (fmap id)
      lhs = compCommutingSquare (joinEqualizerM pair) (mapEqualizerM (unitEqualizerM pair))
      rhs = idCommutingSquare tPair
  in (alpha lhs (EqualizerOb val) === alpha rhs (EqualizerOb val))

prop_monad_natural_right_unit :: Int -> Property
prop_monad_natural_right_unit val =
  let pair = DoubleArrow id id
      tPair = DoubleArrow (fmap id) (fmap id)
      lhs = compCommutingSquare (joinEqualizerM pair) (unitEqualizerM tPair)
      rhs = idCommutingSquare tPair
  in (alpha lhs (EqualizerOb val) === alpha rhs (EqualizerOb val))

prop_monad_natural_associativity :: Int -> Property
prop_monad_natural_associativity val =
  let pair = DoubleArrow id id
      tPair = DoubleArrow (fmap id) (fmap id)
      lhs = compCommutingSquare (joinEqualizerM pair) (mapEqualizerM (joinEqualizerM pair))
      rhs = compCommutingSquare (joinEqualizerM pair) (joinEqualizerM tPair)
      valDouble = EqualizerOb (EqualizerOb (EqualizerOb val))
  in (alpha lhs valDouble === alpha rhs valDouble)

instance (Arbitrary a, Arbitrary b) => Arbitrary (ProductOb a b) where
  arbitrary = (\x y -> ProductOb (x, y)) <$> arbitrary <*> arbitrary


prop_product_properties :: Fun Double Int -> Fun Double String -> Property
prop_product_properties (Fun _ f) (Fun _ g) =
  let diagram = PairObj :: PairObj Int String
      h :: Double -> Int
      h = f
      k :: Double -> String
      k = g
  in case produce diagram of
       Product projA' projB' fact ->
         let val = fact h k 4.2
         in (projA' val === h 4.2)
            .&&.
            (projB' val === k 4.2)

prop_product_functor_identity_law :: ProductOb Int String -> Property
prop_product_functor_identity_law pVal =
  let diagram = PairObj :: PairObj Int String
  in mapProduct (identity diagram) pVal === pVal

prop_product_functor_composition_law
  :: ProductOb Int String
  -> Fun Int Int -> Fun String String
  -> Fun Int Int -> Fun String String
  -> Property
prop_product_functor_composition_law pVal (Fun _ f1) (Fun _ g1) (Fun _ f2) (Fun _ g2) =
  let hom1 = PairMorphism f1 g1
      hom2 = PairMorphism f2 g2
      homComp = compose hom2 hom1
  in mapProduct homComp pVal === mapProduct hom2 (mapProduct hom1 pVal)

prop_product_comonad_extract_law :: ProductOb Int String -> Property
prop_product_comonad_extract_law pVal =
  let d = PairObj :: PairObj Int String
      ext = extractProductM d
      dup = duplicateProductM d
  in mapProduct ext (alphaP dup pVal) === pVal
     .&&.
     mapProduct ext (betaP dup pVal) === pVal

prop_product_comonad_associativity :: ProductOb Int String -> Property
prop_product_comonad_associativity pVal =
  let d = PairObj :: PairObj Int String
      dup = duplicateProductM d
      dup2 = duplicateProductM (PairObj :: PairObj (ProductOb Int String) (ProductOb Int String))
      lhs = alphaP dup2 (alphaP dup pVal)
      rhs = mapProduct dup (alphaP dup pVal)
  in lhs === rhs


limitTests :: TestTree
limitTests = testGroup "Limit and Colimit Tests"
  [ testGroup "Commuting Square Laws"
      [ testProperty "commuting laws hold for constructed square" prop_commuting_square_laws
      , testProperty "composition preserves commuting laws" prop_commuting_square_composition_laws
      ]
  , testGroup "TerminalM Functor Laws"
      [ testProperty "identity: fmap id == id" prop_functor_identity
      , testProperty "composition: fmap (g . f) == fmap g . fmap f" prop_functor_composition
      ]
  , testGroup "Equalizer Properties"
      [ testProperty "commutativity and universality" prop_equalizer_properties
      , testProperty "functor: identity law" prop_functor_identity_law
      , testProperty "functor: composition law" prop_functor_composition_law
      , testProperty "functor: correctness / preservation" prop_functor_correctness_law
      ]
  , testGroup "EqualizerOb Monad Laws"
      [ testProperty "left identity: return x >>= f == f x" prop_eq_monad_left_identity
      , testProperty "right identity: m >>= return == m" prop_eq_monad_right_identity
      , testProperty "associativity: (m >>= f) >>= g == m >>= (\\x -> f x >>= g)" prop_eq_monad_associativity
      ]
  , testGroup "mapEqualizerM Functor Properties"
      [ testProperty "mapEqualizerM preserves composition" prop_mapEqualizerM_composition
      , testProperty "natural monad: left unit law" prop_monad_natural_left_unit
      , testProperty "natural monad: right unit law" prop_monad_natural_right_unit
      , testProperty "natural monad: associativity law" prop_monad_natural_associativity
      ]
  , testGroup "Categorical Product Properties"
      [ testProperty "universality: projections and factorization" prop_product_properties
      , testProperty "functor: identity law" prop_product_functor_identity_law
      , testProperty "functor: composition law" prop_product_functor_composition_law
      , testProperty "comonad: extract law" prop_product_comonad_extract_law
      , testProperty "comonad: associativity law" prop_product_comonad_associativity
      ]
  ]





{-
-- equalizer Eq as a functor on object
newtype DoubleArrow a b = DoubleArrow ((a -> b), (a -> b))
def equalize (DoubleArrow a b) -> (c, c -> b)
-- on object




fmapEq :: (Eq a) => (a -> b) -> (a -> b) 
fmapEq f eqA = eqA 

-- delta eq as a functor
-}


