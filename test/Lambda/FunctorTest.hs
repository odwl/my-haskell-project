module Lambda.FunctorTest where

import Control.Monad.Reader (reader)
import Lambda.Functor
import Lambda.FunctorHelp (eqMyReader, eqReader)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes (functor)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tastyBatch :: TestBatch -> TestTree
tastyBatch (name, tests) = testProperties name tests

-- ==========================================
-- 1. Standard Maybe Tests
-- ==========================================

functorMaybeTests :: TestTree
functorMaybeTests =
  testGroup
    "Functor Maybe"
    [ testCase "Hardcoded Just 10" $ fmap id (Just (10 :: Int)) @?= Just (10 :: Int),
      testCase "Hardcoded Nothing" $ fmap id (Nothing :: Maybe Int) @?= Nothing,
      tastyBatch (functor (undefined :: Maybe (Int, String, Int)))
    ]

-- ==========================================
-- 2. Custom MyMaybe Tests
-- ==========================================

functorMyMaybeTests :: TestTree
functorMyMaybeTests = tastyBatch $ functor (undefined :: MyMaybe (Int, String, Int))

-- ==========================================
-- 3. Standard Reader (Function) Tests
-- ==========================================

prop_readerIdentity :: Fun Int String -> Int -> Property
prop_readerIdentity (Fn rawR) x =
  let r = reader rawR
   in eqReader (fmap id r) r x

prop_readerComposition :: Fun String Int -> Fun Int String -> Fun Int Int -> Int -> Property
prop_readerComposition (Fn f) (Fn g) (Fn rawR) val =
  let r = reader rawR
      leftSide = fmap (f . g) r
      rightSide = (fmap f . fmap g) r
   in eqReader leftSide rightSide val

functorReaderTests :: TestTree
functorReaderTests =
  testGroup
    "Functor Reader"
    [ testProperty "Reader Identity Law" prop_readerIdentity,
      testProperty "Composition Law" prop_readerComposition
    ]

-- ==========================================
-- 4. Custom MyReader Tests
-- ==========================================

prop_myReaderIdentity :: Fun Int String -> Int -> Property
prop_myReaderIdentity (Fn rawR) val =
  let r = MyReader rawR
   in eqMyReader (fmap id r) r val

prop_myReaderComposition :: Fun String Int -> Fun Int String -> Fun Int Int -> Int -> Property
prop_myReaderComposition (Fn f) (Fn g) (Fn rawR) val =
  let r = MyReader rawR
      leftSide = fmap (f . g) r
      rightSide = (fmap f . fmap g) r
   in eqMyReader leftSide rightSide val

functorMyReaderTests :: TestTree
functorMyReaderTests =
  testGroup
    "Functor MyReader"
    [ testProperty "Identity Law" prop_myReaderIdentity,
      testProperty "Composition Law" prop_myReaderComposition
    ]

-- ==========================================
-- Master Test Tree
-- ==========================================

functorTests :: TestTree
functorTests =
  testGroup
    "Functor Suite"
    [ functorMaybeTests,
      functorMyMaybeTests,
      functorReaderTests,
      functorMyReaderTests
    ]
