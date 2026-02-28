module Lambda.StateTest (stateTests) where

import Lambda.State
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperties)

instance (Arbitrary a) => Arbitrary (Expr a) where
  arbitrary = sized expr'
    where
      expr' 0 = Var <$> arbitrary
      expr' n = oneof [Var <$> arbitrary, Add <$> expr' (n `div` 2) <*> expr' (n `div` 2)]

instance (Eq a) => EqProp (Expr a) where
  (=-=) = eq

stateTests :: TestTree
stateTests =
  testGroup
    "State Monad Tests"
    [ testCase "addOne example" $ runState addOne (10 :: Int) @?= (11, 11),
      testCase "get returns state" $ runState get (42 :: Int) @?= (42, 42),
      testCase "put updates state" $ runState (put (100 :: Int)) (42 :: Int) @?= ((), 100),
      testCase "modify updates state with function" $ runState (modify (* (2 :: Int))) (21 :: Int) @?= ((), 42),
      testCase "evalState returns only result" $ evalState addOne (10 :: Int) @?= 11,
      testCase "execState returns only final state" $ execState addOne (10 :: Int) @?= 11,
      testGroup
        "Complex composition"
        [ testCase "multiple steps" $ runState complexCalc 10 @?= (31, 31)
        ],
      testGroup
        "Expr Instances"
        [ testBatch (functor (undefined :: Expr (Int, Int, Int))),
          testBatch (applicative (undefined :: Expr (Int, Int, Int))),
          testBatch (monad (undefined :: Expr (Int, Int, Int)))
        ]
    ]
  where
    testBatch (name, tests) = testProperties name tests

-- Discussion example
addOne :: State Int Int
addOne = do
  current <- get
  let next = current + 1
  put next
  return next

-- Complex test case
complexCalc :: State Int Int
complexCalc = do
  modify (* 2) -- 10 -> 20
  modify (+ 10) -- 20 -> 30
  addOne -- 30 -> 31
