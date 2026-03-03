module Lambda.MiniWhileTest (miniWhileTests) where

import Data.List (intercalate)
import qualified Data.Map as Map
import Lambda.MiniWhile
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

miniWhileTests :: TestTree
miniWhileTests =
  testGroup
    "MiniWhite Parser Tests"
    [ testGroup
        "QuickCheck Properties"
        [ testProperty "parseString handles valid assignment" prop_parse_assign,
          testProperty "parseString handles multiple statements" prop_parse_stmts,
          testProperty "parseString handles addition expressions" prop_parse_addition
        ],
      testGroup
        "Manual Examples"
        [ testCase "Simple assignment" $
            parseString "x := 10" @?= Just (Program [Asgn "x" (Num 10)]),
          testCase "Assignment with addition" $
            parseString "y := (x + 5)" @?= Just (Program [Asgn "y" (Add (Var "x") (Num 5))]),
          testCase "Multi-statement program" $
            parseString "x := 1; y := 2; z := (x + y)"
              @?= Just
                ( Program
                    [ Asgn "x" (Num 1),
                      Asgn "y" (Num 2),
                      Asgn "z" (Add (Var "x") (Var "y"))
                    ]
                )
        ],
      testGroup
        "Interpreter Tests"
        [ testCase "Basic evaluation" $
            evalProgram (Program [Asgn "x" (Num 5)]) @?= Map.singleton "x" 5,
          testCase "Sequence evaluation" $
            evalProgram (Program [Asgn "x" (Num 5), Asgn "y" (Add (Var "x") (Num 10))])
              @?= Map.fromList [("x", 5), ("y", 15)],
          testCase "Default value for uninitialized variable" $
            evalProgram (Program [Asgn "y" (Add (Var "x") (Num 1))])
              @?= Map.fromList [("y", 1)]
        ]
    ]

-- ==========================================
-- Generators
-- ==========================================

genId :: Gen Id
genId = do
  c <- elements ['a' .. 'z']
  cs <- listOf $ elements (['a' .. 'z'] ++ ['0' .. '9'])
  return (c : cs)

genExp :: Int -> Gen Exp
genExp 0 = oneof [Num <$> arbitrarySizedNatural, Var <$> genId]
genExp n =
  oneof
    [ Num <$> arbitrarySizedNatural,
      Var <$> genId,
      Add <$> genExp (n `div` 2) <*> genExp (n `div` 2)
    ]

genStmt :: Int -> Gen Stmt
genStmt n = Asgn <$> genId <*> genExp n

genProgram :: Int -> Gen Program
genProgram n = Program <$> listOf1 (genStmt (n `div` 2))

-- ==========================================
-- Pretty Printers for Generation
-- ==========================================

renderExp :: Exp -> String
renderExp (Num n) = show n
renderExp (Var i) = i
renderExp (Add e1 e2) = "(" ++ renderExp e1 ++ " + " ++ renderExp e2 ++ ")"

renderStmt :: Stmt -> String
renderStmt (Asgn i e) = i ++ " := " ++ renderExp e

renderProgram :: Program -> String
renderProgram (Program stmts) = intercalate "; " (map renderStmt stmts)

-- ==========================================
-- Properties
-- ==========================================

prop_parse_assign :: Property
prop_parse_assign = forAll (genStmt 2) $ \stmt ->
  let input = renderStmt stmt
      expected = Program [stmt]
   in parseString input == Just expected

prop_parse_stmts :: Property
prop_parse_stmts = forAll (genProgram 4) $ \prog ->
  let input = renderProgram prog
   in parseString input == Just prog

prop_parse_addition :: Property
prop_parse_addition = forAll (genExp 3) $ \e ->
  let input = "x := " ++ renderExp e
      expected = Program [Asgn "x" e]
   in parseString input == Just expected
