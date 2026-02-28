{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lambda.StateTest (stateTests) where

import Data.Foldable (toList)
import Data.Maybe (fromJust, isNothing)
import Lambda.State
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperties, testProperty)

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
        ],
      testGroup
        "replace function"
        [ testProperty "no matches (0% overlap)" prop_replace_no_matches,
          testProperty "all matches (100% overlap)" prop_replace_all_matches
        ],
      testGroup
        "convert function"
        [ testProperty "correct traversal logic" prop_convert_logic
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

prop_replace_no_matches :: Expr Int -> [Int] -> Property
prop_replace_no_matches expr vals =
  forAll (genDisjointDict expr vals) $ \dict ->
    replace dict expr === (Nothing <$ expr)

prop_replace_all_matches :: [(Int, Int)] -> Expr Int -> Property
prop_replace_all_matches dict rawExpr =
  not (null dict) ==>
    let keys = map fst dict
        expr = (\x -> keys !! (abs x `mod` length keys)) <$> rawExpr
     in replace dict expr === fmap (`lookup` dict) expr

-- | Generates a dictionary by zipping a list of values with keys guaranteed to be absent from the expression.
genDisjointDict :: (Arbitrary a, Eq a) => Expr a -> [b] -> Gen [(a, b)]
genDisjointDict expr vals = do
  let forbidden = toList expr
  keys <- vectorOf (length vals) (arbitrary `suchThat` (`notElem` forbidden))
  return (zip keys vals)

prop_convert_logic :: Expr (Maybe Int) -> Property
prop_convert_logic expr =
  let res = convert expr
   in if any isNothing expr
        then classify True "result is Nothing" $ res === Nothing
        else classify True "result is Just" $ res === Just (fromJust <$> expr)
