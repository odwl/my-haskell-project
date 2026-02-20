{-# LANGUAGE FlexibleInstances #-}
module Lambda.FunctorTest where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.Show.Functions
import Lambda.Functor (
    MyMaybe(..),
    MyReader(..), runMyReader)
import Control.Monad.Reader

-- ==========================================
-- 1. Standard Maybe Tests
-- ==========================================
prop_maybeIdentity :: Maybe Int -> Bool
prop_maybeIdentity m = fmap id m == m

prop_maybeComposition :: (Int -> Int) -> (Int -> Int) -> Maybe Int -> Bool
prop_maybeComposition f g m = fmap (f . g) m == (fmap f . fmap g) m

functorMaybeTests :: TestTree
functorMaybeTests = testGroup "Functor Maybe"
    [ testCase "Hardcoded Just 10" $ fmap id (Just 10) @?= Just 10
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
        , (3, fmap pure arbitrary)
        ]

prop_myFunctorIdentity :: MyMaybe String -> Bool
prop_myFunctorIdentity m = fmap id m == m

prop_myFunctorComposition :: (Int -> String) -> (String -> Int) -> MyMaybe String -> Bool
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

-- A custom operator for "Extensional Equality" of Readers
eqReader :: (Eq a, Show a) => Reader r a -> Reader r a -> r -> Property
eqReader r1 r2 x = runReader r1 x === runReader r2 x

prop_readerIdentity :: Fun Int Int -> Int -> Property
prop_readerIdentity (Fn rawR) x =  
    let r = reader rawR
    in eqReader (fmap id r) r x

prop_readerComposition :: Fun String Int -> Fun Int String -> Fun Int Int -> Int -> Property
prop_readerComposition (Fn f) (Fn g) (Fn rawR) val =
    let r = reader rawR
        leftSide  = fmap (f . g) r
        rightSide = (fmap f . fmap g) r
    in eqReader leftSide rightSide val

functorReaderTests :: TestTree
functorReaderTests = testGroup "functor reader id"
    [ testCase "hardcoded (+1) 5" $
        let rdr = reader (+1)
        in runReader (fmap id rdr) 5 @?= runReader rdr 5
    , testProperty "Reader Identity Law" prop_readerIdentity
    , testProperty "Composition Law" prop_readerComposition
      ]

-- ==========================================
-- 4. Custom MyReader Tests
-- ==========================================

eqMyReader :: (Eq b, Show b) => MyReader a b -> MyReader a b -> a -> Property
eqMyReader m1 m2 x = runMyReader m1 x === runMyReader m2 x

-- pattern FnReader :: MyReader a b -> Fun a b
-- pattern FnReader r <- (MyReader . applyFun -> r)

prop_myReaderIdentity :: Fun Int String -> Int -> Property
prop_myReaderIdentity fun val =
    let r = MyReader (applyFun fun)
    in eqMyReader (fmap id r) r val

prop_myReaderComposition :: Fun String Int -> Fun Int String -> Fun Int Int -> Int -> Property
prop_myReaderComposition (Fn f) (Fn g) (Fn rawR) val =
    let r = MyReader rawR
        leftSide  = fmap (f . g) r
        rightSide = (fmap f . fmap g) r
    in eqMyReader leftSide rightSide val

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
