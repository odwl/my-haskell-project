{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lambda.FunctorTest where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.Show.Functions ()
-- import Lambda
import Lambda.Functor (
    MyMaybe(..),
    MyReader(..), runMyReader)
import Control.Monad.Reader
import Control.Monad.Identity

-- ==========================================
-- 1. Standard Maybe Tests
-- ==========================================
prop_maybeIdentity :: Maybe Int -> Bool
prop_maybeIdentity m = fmap id m == m

prop_maybeComposition :: (Int -> Int) -> (Int -> Int) -> Maybe Int -> Bool
prop_maybeComposition f g m = fmap (f . g) m == (fmap f . fmap g) m

functorMaybeTests :: TestTree
functorMaybeTests = testGroup "Functor Maybe"
    [ testCase "Hardcoded Just 10" $ fmap id (Just (10 :: Int)) @?= Just 10
    , testCase "Hardcoded Nothing" $ fmap id (Nothing :: Maybe Int) @?= Nothing
    , testProperty "Identity Law" prop_maybeIdentity
    , testProperty "Composition Law" prop_maybeComposition
    ]

-- ==========================================
-- 2. Custom MyMaybe Tests
-- ==========================================
instance Arbitrary a => Arbitrary (MyMaybe a) where
    arbitrary = frequency 
        [ (1, pure MyNothing)
        , (3, MyJust <$> arbitrary)
        ]

prop_myFunctorIdentity :: MyMaybe Int -> Bool
prop_myFunctorIdentity m = fmap id m == m

prop_myFunctorComposition :: (Int -> Int) -> (Int -> Int) -> MyMaybe Int -> Bool
prop_myFunctorComposition f g m = fmap (f . g) m == (fmap f . fmap g) m

functorMyMaybeTests :: TestTree
functorMyMaybeTests = testGroup "Functor MyMaybe"
    [ testCase "Hardcoded MyJust 10" $ fmap id (MyJust (10 :: Int)) @?= MyJust 10
    , testCase "Hardcoded MyNothing" $ fmap id (MyNothing :: MyMaybe Int) @?= MyNothing
    , testProperty "Identity Law" prop_myFunctorIdentity
    , testProperty "Composition Law" prop_myFunctorComposition
    ]

-- ==========================================
-- 3. Standard Reader (Function) Tests
-- ==========================================

prop_readerIdentity :: Reader Int Int -> Int -> Bool
prop_readerIdentity r x = runReader (fmap id r) x == runReader r x

instance (CoArbitrary r, Arbitrary a) => Arbitrary (ReaderT r Identity a) where
    arbitrary = fmap (ReaderT . (Identity .)) arbitrary

prop_readerComposition :: (Int -> Int) -> (Int -> Int) -> Reader Int Int -> Int -> Bool
prop_readerComposition f g r x = 
    let leftSide  = fmap (f . g) r
        rightSide = (fmap f . fmap g) r
    in runReader leftSide x == runReader rightSide x

instance Show (ReaderT r m a) where
    show _ = "<ReaderT function>"

functorReaderTests :: TestTree
functorReaderTests = testGroup "functor reader id"
    [ testCase "hardcoded (+1) 5" $
        let rdr = ReaderT (Identity . (+1))
        in runReader (fmap id rdr) 5 @?= runReader rdr 5
    , testProperty "Reader Identity Law" prop_readerIdentity
    , testProperty "Composition Law" prop_readerComposition
      ]

-- ==========================================
-- 4. Custom MyReader Tests
-- ==========================================
instance (CoArbitrary a, Arbitrary b) => Arbitrary (MyReader a b) where
    arbitrary = fmap MyReader arbitrary -- Or 'mkReader' if using smart constructors

instance Show (MyReader a b) where
    show _ = "<MyReader function>"

eqReader :: Eq b => MyReader a b -> MyReader a b -> a -> Bool
eqReader m1 m2 x = runMyReader m1 x == runMyReader m2 x

prop_myReaderIdentity :: MyReader Int Int -> Int -> Bool
prop_myReaderIdentity r x = eqReader (fmap id r) r x

prop_myReaderComposition :: (Int -> Int) -> (Int -> Int) -> MyReader Int Int -> Int -> Bool
prop_myReaderComposition f g r x = 
    let leftSide  = fmap (f . g) r
        rightSide = (fmap f . fmap g) r
    in eqReader leftSide rightSide x

functorMyReaderTests :: TestTree
functorMyReaderTests = testGroup "Functor MyReader"
    [ testCase "Hardcoded (+1) 5" $ runMyReader (fmap id (MyReader (+1))) 5 @?= (+1) (5 :: Int)
    , testProperty "Identity Law" prop_myReaderIdentity
    , testProperty "Composition Law" prop_myReaderComposition
    ]


-- ==========================================
-- Master Test Tree
-- ==========================================
functorTests :: TestTree
functorTests = testGroup "Functor Suite"
    [ functorMaybeTests
    , functorMyMaybeTests
    , functorReaderTests
    , functorMyReaderTests
    ]
