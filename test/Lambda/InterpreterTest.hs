{-# OPTIONS_GHC -Wno-orphans #-}

module Lambda.InterpreterTest (interpreterTests) where

import Control.Monad.State (evalState, execState)
import qualified Data.Map as Map
import Lambda.Interpreter
import Lambda.Parser
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

interpreterTests :: TestTree
interpreterTests =
  testGroup
    "Interpreter Tests"
    [ testGroup
        "Arithmetic Evaluation"
        [ testCase "Basic addition" $ runEvalA (Op (Lit (VInt 5)) Add (Lit (VInt 10))) @?= VInt 15,
          testCase "Subtraction" $ runEvalA (Op (Lit (VInt 20)) Sub (Lit (VInt 5))) @?= VInt 15,
          testCase "Multiplication" $ runEvalA (Op (Lit (VInt 3)) Mul (Lit (VInt 4))) @?= VInt 12,
          testCase "Division" $ runEvalA (Op (Lit (VInt 20)) Div (Lit (VInt 4))) @?= VInt 5,
          testCase "Division by zero" $ runEvalA (Op (Lit (VInt 10)) Div (Lit (VInt 0))) @?= VInt 0
        ],
      testGroup
        "Expression Evaluation"
        [ testCase "Comparison (Le)" $ runEvalE (Cmp (Lit (VInt 5)) Le (Lit (VInt 10))) @?= VInt 1,
          testCase "Comparison (Gt) false" $ runEvalE (Cmp (Lit (VInt 5)) Gt (Lit (VInt 10))) @?= VInt 0,
          testCase "Not (Bool)" $ runEvalE (Not (EAExp (Lit (VBool True)))) @?= VBool False,
          testCase "If true (Bool)" $ runEvalE (If (EAExp (Lit (VBool True))) (EAExp (Lit (VInt 42))) (EAExp (Lit (VInt 0)))) @?= VInt 42,
          testCase "If true" $ runEvalE (If (EAExp (Lit (VInt 1))) (EAExp (Lit (VInt 42))) (EAExp (Lit (VInt 0)))) @?= VInt 42,
          testCase "If false" $ runEvalE (If (EAExp (Lit (VInt 0))) (EAExp (Lit (VInt 42))) (EAExp (Lit (VInt 7)))) @?= VInt 7
        ],
      testGroup
        "Statement Execution"
        [ testCase "Assignment" $
            let s = Assign (mkI "x") (EAExp (Lit (VInt 10)))
                mem = execState (evalStmt s) emptyMemory
             in Map.lookup (mkI "x") mem @?= Just (VInt 10),
          testCase "While loop (factorial)" $
            -- n := 5; res := 1; while n > 0 do res := res * n; n := n - 1 done
            let n = mkI "n"
                res = mkI "res"
                prog =
                  [ Assign n (EAExp (Lit (VInt 5))),
                    Assign res (EAExp (Lit (VInt 1))),
                    While
                      (Cmp (Var n) Gt (Lit (VInt 0)))
                      [ Assign res (EAExp (Op (Var res) Mul (Var n))),
                        Assign n (EAExp (Op (Var n) Sub (Lit (VInt 1))))
                      ]
                  ]
                mem = execState (evalStmts prog) emptyMemory
             in Map.lookup res mem @?= Just (VInt 120)
        ],
      testGroup
        "Mixed Type Evaluation (Fallback)"
        [ testCase "Add Int and Bool" $ runEvalA (Op (Lit (VInt 5)) Add (Lit (VBool True))) @?= VInt 0,
          testCase "Compare Int and Bool" $ runEvalE (Cmp (Lit (VInt 5)) Eq (Lit (VBool True))) @?= VInt 0,
          testCase "If with Int 0" $ runEvalE (If (EAExp (Lit (VInt 0))) (EAExp (Lit (VInt 1))) (EAExp (Lit (VInt 2)))) @?= VInt 2,
          testCase "If with Int 1" $ runEvalE (If (EAExp (Lit (VInt 1))) (EAExp (Lit (VInt 1))) (EAExp (Lit (VInt 2)))) @?= VInt 1
        ],
      testGroup
        "QuickCheck Properties"
        [ testProperty "Integer arithmetic results in VInt" prop_arith_vint,
          testProperty "Mixed arithmetic results in VInt 0" prop_mixed_arith_zero
        ]
    ]
  where
    runEvalA a = evalState (evalAExp a) emptyMemory
    runEvalE e = evalState (evalExp e) emptyMemory
    mkI (c : cs) = case mkId c cs of Just y -> y; Nothing -> error ""
    mkI [] = error "Empty ID"

prop_arith_vint :: Int -> Int -> BinOp -> Property
prop_arith_vint n1 n2 op =
  let res = evalState (evalAExp (Op (Lit (VInt n1)) op (Lit (VInt n2)))) Map.empty
   in property $ case res of
        VInt _ -> True
        _ -> False

prop_mixed_arith_zero :: Int -> Bool -> BinOp -> Property
prop_mixed_arith_zero n b op =
  let res1 = evalState (evalAExp (Op (Lit (VInt n)) op (Lit (VBool b)))) Map.empty
      res2 = evalState (evalAExp (Op (Lit (VBool b)) op (Lit (VInt n)))) Map.empty
   in res1 === VInt 0 .&&. res2 === VInt 0

instance Arbitrary BinOp where
  arbitrary = elements [Add, Sub, Mul, Div]
