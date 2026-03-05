module Lambda.InterpreterTest (interpreterTests) where

import Control.Monad.State (evalState, execState)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Lambda.Interpreter
import Lambda.Parser
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck ()

interpreterTests :: TestTree
interpreterTests =
  testGroup
    "Interpreter Tests"
    [ testGroup
        "Arithmetic Evaluation"
        [ testCase "Basic addition" $ runEvalA (Op (Num 5) Add (Num 10)) @?= 15,
          testCase "Subtraction" $ runEvalA (Op (Num 20) Sub (Num 5)) @?= 15,
          testCase "Multiplication" $ runEvalA (Op (Num 3) Mul (Num 4)) @?= 12,
          testCase "Division" $ runEvalA (Op (Num 20) Div (Num 4)) @?= 5,
          testCase "Division by zero" $ runEvalA (Op (Num 10) Div (Num 0)) @?= 0
        ],
      testGroup
        "Expression Evaluation"
        [ testCase "Comparison (Le)" $ runEvalE (Cmp (Num 5) Le (Num 10)) @?= 1,
          testCase "Comparison (Gt) false" $ runEvalE (Cmp (Num 5) Gt (Num 10)) @?= 0,
          testCase "Not" $ runEvalE (Not (EAExp (Num 0))) @?= 1,
          testCase "If true" $ runEvalE (If (EAExp (Num 1)) (EAExp (Num 42)) (EAExp (Num 0))) @?= 42,
          testCase "If false" $ runEvalE (If (EAExp (Num 0)) (EAExp (Num 42)) (EAExp (Num 7))) @?= 7
        ],
      testGroup
        "Statement Execution"
        [ testCase "Assignment" $
            let s = Assign (mkI "x") (EAExp (Num 10))
                mem = execState (evalStmt s) emptyMemory
             in Map.lookup (mkI "x") mem @?= Just 10,
          testCase "While loop (factorial)" $
            -- n := 5; res := 1; while n > 0 do res := res * n; n := n - 1 done
            let n = mkI "n"
                res = mkI "res"
                prog =
                  [ Assign n (EAExp (Num 5)),
                    Assign res (EAExp (Num 1)),
                    While
                      (Cmp (Var n) Gt (Num 0))
                      [ Assign res (EAExp (Op (Var res) Mul (Var n))),
                        Assign n (EAExp (Op (Var n) Sub (Num 1)))
                      ]
                  ]
                mem = execState (evalStmts prog) emptyMemory
             in Map.lookup res mem @?= Just 120
        ]
    ]
  where
    runEvalA a = evalState (evalAExp a) emptyMemory
    runEvalE e = evalState (evalExp e) emptyMemory
    mkI (c : cs) = fromJust $ mkId c cs
    mkI [] = error "Empty ID"
