module Main where

import Lambda
import Lambda.FunctorTest (functorTests)
import Lambda.MonadTest (monadTests)
import Test.Tasty
import Test.Tasty.HUnit

testDoubleApplication :: TestTree
testDoubleApplication =
  testGroup
    "Double Apply Tests"
    [testCase "Apply (+1) twice" $ applyTwice (+ 1) (10 :: Int) @?= (12 :: Int)]

safeDivTests :: TestTree
safeDivTests =
  testGroup
    "SafeDiv Tests"
    [ testCase "Divides evenly" $ safeDiv 10 2 @?= Just 5,
      testCase "Division by zero yields Nothing" $ safeDiv 10 0 @?= Nothing,
      testCase "Integer division truncates" $ safeDiv 10 3 @?= Just 3,
      testCase "Zero divided by number is zero" $ safeDiv 0 10 @?= Just 0
    ]

yFactTests :: TestTree
yFactTests =
  testGroup
    "YFact"
    [ testCase "fact 0" $ fact 0 @?= 1,
      testCase "fact 1" $ fact 1 @?= 1,
      testCase "fact 2" $ fact 2 @?= 2,
      testCase "fact 3" $ fact 3 @?= 6
    ]

safeHeadTests :: TestTree
safeHeadTests =
  testGroup
    "SafeHead"
    [ testCase "empty list" $ safeHead ([] :: [Int]) @?= Nothing,
      testCase "multiple elements" $ safeHead ([1, 2, 3] :: [Int]) @?= Just (1 :: Int),
      testCase "single element" $ safeHead ([10] :: [Int]) @?= Just (10 :: Int)
    ]

addMaybesTests :: TestTree
addMaybesTests =
  testGroup
    "AddMaybe"
    [ testCase "Just + Just" $ addMaybes (Just 1) (Just 1) @?= Just 2,
      testCase "Nothing + Just" $ addMaybes Nothing (Just 1) @?= Nothing,
      testCase "Just + Nothing" $ addMaybes (Just 1) Nothing @?= Nothing,
      testCase "Nothing + Nothing" $ addMaybes Nothing Nothing @?= Nothing
    ]

tests :: TestTree
tests =
  testGroup
    "Lambda Suite"
    [ addMaybesTests,
      safeHeadTests,
      yFactTests,
      testDoubleApplication,
      safeDivTests,
      functorTests,
      monadTests
    ]

main :: IO ()
main = do
  putStrLn "\n--- Running: Lambda Suite ---"
  defaultMain tests
