{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Lambda.FreeTest (freeTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Control.Comonad (Comonad (..))
import Lambda.Free
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = (:|) <$> arbitrary <*> arbitrary

instance (Arbitrary a) => Arbitrary (Cofree Maybe a) where
  arbitrary = sized arbCofree
    where
      arbCofree 0 = (:< Nothing) <$> arbitrary
      arbCofree n = do
        val <- arbitrary
        children <- frequency [(1, pure Nothing), (3, Just <$> arbCofree (n `div` 2))]
        return (val :< children)

-- | Property: Cofree Maybe comonad left identity: extract . duplicate = id
prop_cofree_left_identity :: Cofree Maybe Int -> Property
prop_cofree_left_identity c = extract (duplicate c) === c

-- | Property: Cofree Maybe comonad right identity: fmap extract . duplicate = id
prop_cofree_right_identity :: Cofree Maybe Int -> Property
prop_cofree_right_identity c = fmap extract (duplicate c) === c

-- | Property: Cofree Maybe comonad associativity: duplicate . duplicate = fmap duplicate . duplicate
prop_cofree_associativity :: Cofree Maybe Int -> Property
prop_cofree_associativity c = duplicate (duplicate c) === fmap duplicate (duplicate c)

-- | Free Semigroup adjunction properties
prop_counit_homo :: NonEmpty String -> NonEmpty String -> Property
prop_counit_homo xs ys = 
    counitSemigroup (xs <> ys) === counitSemigroup xs <> counitSemigroup ys

prop_triangle1 :: String -> Property
prop_triangle1 x = 
    counitSemigroup (unitSemigroup x) === x

prop_triangle2 :: NonEmpty Int -> Property
prop_triangle2 xs = 
    counitSemigroup (NE.map unitSemigroup xs) === xs

-- | Free Monoid adjunction properties
prop_counitMonoid_homo :: [String] -> [String] -> Property
prop_counitMonoid_homo xs ys = 
    counitMonoid (xs <> ys) === counitMonoid xs <> counitMonoid ys

prop_counitMonoid_mempty :: Property
prop_counitMonoid_mempty = 
    counitMonoid [] === (mempty :: String)

prop_triangleMonoid1 :: String -> Property
prop_triangleMonoid1 x = 
    counitMonoid (unitMonoid x) === x

prop_triangleMonoid2 :: [Int] -> Property
prop_triangleMonoid2 xs = 
    counitMonoid (map unitMonoid xs) === xs

freeTests :: TestTree
freeTests = testGroup "Free, Cofree and Coyoneda Tests"
  [ testGroup "Free Functor - Coyoneda"
      [ testCase "lift and lower Coyoneda" $ do
          let maybeVal = Just (42 :: Int)
              coyonedaVal = liftCoyoneda maybeVal
              mapped = fmap (+1) coyonedaVal
          lowerCoyoneda mapped @?= Just 43
      , testCase "ContraCoyoneda len2 example" $ do
          len2 (1234 :: Int) @?= 4
      ]
  , testGroup "Free Monad - Teletype DSL"
      [ testCase "runTeletypePure interaction" $ do
          let program = do
                putStrLnF "Hello"
                name <- getLineF
                putStrLnF ("Welcome, " ++ name)
                return 100
              (outputs, res) = runTeletypePure ["World"] program
          outputs @?= ["Hello", "Welcome, World"]
          res @?= (100 :: Int)
      ]
  , testGroup "Cofree Comonad"
      [ testCase "extract and unwrap examples" $ do
          extract exampleCofree1 @?= 3
          unwrap exampleCofree1 @?= Just (5 :< Nothing)
      , testProperty "Cofree Maybe extract . duplicate = id" prop_cofree_left_identity
      , testProperty "Cofree Maybe fmap extract . duplicate = id" prop_cofree_right_identity
      , testProperty "Cofree Maybe duplicate . duplicate = fmap duplicate . duplicate" prop_cofree_associativity
      ]
  , testGroup "Free Semigroup Adjunction"
      [ testProperty "counit is a homomorphism" prop_counit_homo
      , testProperty "counit . unit == id" prop_triangle1
      , testProperty "counit . map unit == id" prop_triangle2
      ]
  , testGroup "Free Monoid Adjunction"
      [ testProperty "counitMonoid is a homomorphism (mappend)" prop_counitMonoid_homo
      , testProperty "counitMonoid is a homomorphism (mempty)" prop_counitMonoid_mempty
      , testProperty "counitMonoid . unitMonoid == id" prop_triangleMonoid1
      , testProperty "counitMonoid . map unitMonoid == id" prop_triangleMonoid2
      ]
  ]
