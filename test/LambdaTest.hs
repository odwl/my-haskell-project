module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.Show.Functions ()
import Lambda

testDoubleApplication :: TestTree
testDoubleApplication = testGroup "Double Apply Tests"
    [ testCase "Apply (+1) twice" $ applyTwice (+1) 10  @?= 12 ]

safeDivTests :: TestTree
safeDivTests = testGroup "SafeDiv Tests"
    [ testCase "Divides evenly"                  $ safeDiv 10 2 @?= Just 5
    , testCase "Division by zero yields Nothing" $ safeDiv 10 0 @?= Nothing
    , testCase "Integer division truncates"      $ safeDiv 10 3 @?= Just 3
    , testCase "Zero divided by number is zero"  $ safeDiv 0 10 @?= Just 0
    ]

yFactTests :: TestTree
yFactTests = testGroup "YFact"
    [ testCase "fact 0" $ fact 0 @?= 1
    , testCase "fact 1" $ fact 1 @?= 1
    , testCase "fact 2" $ fact 2 @?= 2
    , testCase "fact 3" $ fact 3 @?= 6
    ]

safeHeadTests :: TestTree
safeHeadTests = testGroup "SafeHead"
    [ testCase "empty list"        $ safeHead ([] :: [Int]) @?= Nothing
    , testCase "multiple elements" $ safeHead [1,2,3]       @?= Just 1
    , testCase "single element"    $ safeHead [10]          @?= Just 10 
    ]

addMaybesTests :: TestTree
addMaybesTests = testGroup "AddMaybe"
    [ testCase "Just + Just"       $ addMaybes (Just 1) (Just 1) @?= Just 2
    , testCase "Nothing + Just"    $ addMaybes Nothing (Just 1) @?= Nothing
    , testCase "Just + Nothing"    $ addMaybes (Just 1) Nothing @?= Nothing
    , testCase "Nothing + Nothing" $ addMaybes Nothing Nothing @?= Nothing
    ]

prop_functorIdentity :: Maybe Int -> Bool
prop_functorIdentity m = fmap id m == m

functorMaybeTests :: TestTree
functorMaybeTests = testGroup "functor maybe id"
    [ testCase "Hardcoded Just 10" $ fmap id (Just (10 :: Int)) @?= Just 10,
      testCase "Hardcoded Nothing" $ fmap id (Nothing :: Maybe Int) @?= Nothing,
      testProperty "Functor Identity Law" prop_functorIdentity]

prop_readerIdentity :: (Int -> Int) -> Int -> Bool
prop_readerIdentity m x = (fmap id m) x == m x

functorReaderTests :: TestTree
functorReaderTests = testGroup "functor reader id"
    [ testCase "hardcoded (+1) 5" $ fmap id (+1) 5 @?= (+1) 5
    , testProperty "Reader Identity Law" prop_readerIdentity
      ]

tests :: TestTree
tests = testGroup "Lambda Suite"
    [ addMaybesTests
    , safeHeadTests
    , yFactTests
    , testDoubleApplication
    , safeDivTests
    , functorMaybeTests
    , functorReaderTests
    ]

main :: IO ()
main = do
    putStrLn "\n--- Running: Lambda Suite ---"
    defaultMain tests
