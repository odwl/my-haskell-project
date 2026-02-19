module Lambda.FunctorTest where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.Show.Functions ()
import Lambda
import Lambda.Functor (MyReader(..), runMyReader)


prop_functorIdentity :: Maybe Int -> Bool
prop_functorIdentity m = fmap id m == m

functorMaybeTests :: TestTree
functorMaybeTests = testGroup "functor maybe id"
    [ testCase "Hardcoded Just 10" $ fmap id (Just (10 :: Int)) @?= Just 10,
      testCase "Hardcoded Nothing" $ fmap id (Nothing :: Maybe Int) @?= Nothing,
      testProperty "Functor Identity Law" prop_functorIdentity]


prop_myFunctorIdentity :: MyMaybe Int -> Bool
prop_myFunctorIdentity m = fmap id m == m

instance Arbitrary a => Arbitrary (MyMaybe a) where
    arbitrary = frequency 
        [ (1, return MyNothing)
        , (3, MyJust <$> arbitrary)
        ]

functorMyMaybeTests :: TestTree
functorMyMaybeTests = testGroup "functor MyMaybe id"
    [ testCase "Hardcoded MyJust 10" $ fmap id (MyJust (10 :: Int)) @?= MyJust 10,
      testCase "Hardcoded MyNothing" $ fmap id (MyNothing :: MyMaybe Int) @?= MyNothing,
      testProperty "MyMaybe Functor Identity Law" prop_myFunctorIdentity
      ]

prop_readerIdentity :: (Int -> Int) -> Int -> Bool
prop_readerIdentity m x = (fmap id m) x == m x

functorReaderTests :: TestTree
functorReaderTests = testGroup "functor reader id"
    [ testCase "hardcoded (+1) 5" $ fmap id (+1) 5 @?= (+1) 5
    , testProperty "Reader Identity Law" prop_readerIdentity
      ]

instance (CoArbitrary a, Arbitrary b) => Arbitrary (MyReader a b) where
    arbitrary = fmap MyReader arbitrary

instance Show (MyReader a b) where
    show _ = "<MyReader function>"

eqReader :: Eq b => MyReader a b -> MyReader a b -> a -> Bool
eqReader m1 m2 x = runMyReader m1 x == runMyReader m2 x

prop_myReaderIdentity :: MyReader Int Int -> Int -> Bool
prop_myReaderIdentity r x = eqReader (fmap id r) r x

functorMyReaderTests :: TestTree
functorMyReaderTests = testGroup "functor myreader id"
    [ testCase "hardcoded (+1) 5" $ runMyReader (fmap id (MyReader (+1))) 5 @?= (+1) (5 :: Int)
    , testProperty "Reader Identity Law" prop_myReaderIdentity ]

functorTests :: TestTree
functorTests = testGroup "Lambda Suite"
    [ 
      functorMaybeTests
    , functorMyMaybeTests
    , functorReaderTests
    , functorMyReaderTests
    ]

main :: IO ()
main = do
    putStrLn "\n--- Running: Functor Suite ---"
    defaultMain functorTests