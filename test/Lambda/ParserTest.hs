{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lambda.ParserTest (parserTests) where

import Control.Applicative (Alternative (..))
import Data.Bifunctor (first, second)
import Data.Char (isAlpha, isAlphaNum, isAscii, isDigit, isPunctuation, isSpace, isSymbol)
import Data.List (intercalate, isPrefixOf)
import Data.Maybe (fromJust)
import Lambda.Parser (AExp (..), BinOp (..), CmpOp (..), Exp (..), Id, ReadP, Stmt (..), aexp, binop, cmpop, expr, identifier, mkId, num, stmt, stmts)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes (applicative, functor, monad)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.ParserCombinators.ReadP (char, pfail, readP_to_S, satisfy, string)

-- | Run parser and return the first result
runReadP :: ReadP a -> String -> Maybe (a, String)
runReadP p s = case readP_to_S p s of
  [] -> Nothing
  ((x, s') : _) -> Just (x, s')

parserTests :: TestTree
parserTests =
  testGroup
    "Parser Suite"
    ( [ testCase "satisfy matches character" $ parseA "abc" @?= Just ('a', "bc"),
        testCase "satisfy does not match character" $ parseA "xbc" @?= Nothing,
        testCase "satisfy on empty string" $ parseA "" @?= Nothing,
        testCase "fmap maps ReadP Char to ReadP String" $ runReadP ((: []) <$> satisfy (== 'a')) "abc" @?= Just ("a", "bc"),
        testCase "fmap maps with custom lambda" $ runReadP ((:) <*> (: []) <$> satisfy (== 'a')) "abc" @?= Just ("aa", "bc")
      ]
        ++ quickTests
        ++ [ tastyBatch
               (functor (undefined :: ReadP (Int, String, Int))),
             tastyBatch
               (applicative (undefined :: ReadP (Int, String, Int))),
             tastyBatch
               (monad (undefined :: ReadP (Int, String, Int)))
           ]
    )
  where
    parseA = runReadP (satisfy (== 'a'))
    tastyBatch (name, tests) = testProperties name tests

quickTests :: [TestTree]
quickTests =
  [ testGroup
      "QuickCheck"
      [ testProperty "satisfy strictly matches character" prop_satisfiesMatchingChar,
        testProperty "satisfy rejects non-matching character" prop_nonMatchingChar,
        testProperty "satisfy rejects empty string" prop_emptyString,
        testProperty "Alternative <|> tries second parser on failure" prop_alternative_choice,
        testProperty "Alternative empty always fails" prop_alternative_empty,
        testProperty "Alternative Law: Left Identity" prop_alternative_left_identity,
        testProperty "Alternative Law: Right Identity" prop_alternative_right_identity,
        testProperty "Alternative is left-biased" prop_alternative_left_bias
      ],
    testGroup
      "AST Parsing Properties"
      [ testProperty "num parses sequence of digits" prop_num_valid,
        testProperty "num rejects invalid sequence" prop_num_invalid,
        testProperty "identifier parses valid names" prop_identifier_valid,
        testProperty "identifier rejects names with invalid start" prop_identifier_reject_invalid_start,
        testProperty "aexp parses numbers and identifiers" prop_aexp_valid,
        testProperty "aexp rejects invalid sequence" prop_aexp_invalid,
        testProperty "expr parses valid exp" prop_expr_valid,
        testProperty "expr rejects invalid sequence" prop_expr_invalid,
        testProperty "binop parses valid operators" prop_binop_valid,
        testProperty "binop rejects invalid operators" prop_binop_invalid,
        testProperty "cmpop parses valid operators" prop_cmpop_valid,
        testProperty "cmpop rejects invalid operators" prop_cmpop_invalid,
        testProperty "stmt matches valid assignment" prop_stmt_valid,
        testProperty "stmt rejects invalid assignment" prop_stmt_invalid,
        testProperty "stmts parses valid sequence" prop_stmts_valid,
        testProperty "stmts rejects invalid sequence" prop_stmts_invalid
      ],
    testGroup
      "Test Examples"
      [ testProperty "stmts parses one assign" prop_stmt_x_2,
        testProperty "stmt parses two assigns" prop_stmt_semi,
        testProperty "stmts parses complex program" prop_stmt_complex
      ]
  ]

instance (Arbitrary a) => Arbitrary (ReadP a) where
  arbitrary = do
    b <- arbitrary
    -- Generate a parser that either succeeds with a value and arbitrary leftover string (pure), or fails
    elements
      [ pfail,
        pure b
      ]

instance (Show a) => Show (ReadP a) where
  show _ = "<Parser>"

instance (Eq a, Show a) => EqProp (ReadP a) where
  p1 =-= p2 =
    property $ \s -> runReadP p1 s === runReadP p2 s

prop_satisfiesMatchingChar :: Char -> String -> Property
prop_satisfiesMatchingChar c s =
  runReadP (satisfy (== c)) (c : s) === Just (c, s)

prop_nonMatchingChar :: Char -> Char -> String -> Property
prop_nonMatchingChar c1 c2 s =
  c1 /= c2 ==> runReadP (satisfy (== c1)) (c2 : s) === Nothing

prop_emptyString :: Char -> Property
prop_emptyString c =
  runReadP (satisfy (== c)) "" === Nothing

prop_alternative_choice :: String -> Property
prop_alternative_choice s =
  runReadP (char 'a' <|> char 'b') ("b" ++ s) === Just ('b', s)

prop_alternative_empty :: String -> Property
prop_alternative_empty s =
  runReadP (empty :: ReadP Int) s === Nothing

prop_alternative_left_identity :: ReadP Int -> String -> Property
prop_alternative_left_identity p s =
  runReadP (empty <|> p) s === runReadP p s

prop_alternative_right_identity :: ReadP Int -> String -> Property
prop_alternative_right_identity p s =
  runReadP (p <|> empty) s === runReadP p s

prop_alternative_left_bias :: String -> Property
prop_alternative_left_bias s =
  runReadP (string "a" <|> string "ab") ("ab" ++ s) === Just ("a", "b" ++ s)

-- ==========================================
--  Generators
-- ==========================================

genSpaces :: Gen String
genSpaces = elements $ "" : map pure " \t\n\r\f\v"

genPadding :: Gen (String, String)
genPadding = do
  spaces <- genSpaces
  sep <- arbitrary `suchThat` (\c -> (isPunctuation c || isSymbol c) && c `notElem` "+-*/=<>!:;")
  rest <- arbitrary
  return (spaces, sep : rest)

withPadding :: Gen (String, a) -> Gen (String, a, String)
withPadding gen = do
  (str, ast) <- gen
  (spaces, rest) <- genPadding
  return (str ++ spaces, ast, rest)

genValidNum :: Gen (String, Int)
genValidNum = (\n -> (show n, n)) <$> arbitrary `suchThat` (>= 0)

genInvalidNum :: Gen String
genInvalidNum = pure <$> arbitrary `suchThat` (not . isDigit)

genValidOp :: [(String, a)] -> Gen (String, a)
genValidOp ops = elements ops

genInvalidOp :: [String] -> Gen String
genInvalidOp invalidPrefixes = do
  s <-
    arbitrary `suchThat` \str ->
      not (null str) && not (isSpace (head str)) && not (any (`isPrefixOf` str) invalidPrefixes)
  return s

isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = isAscii c && isAlpha c

isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = isAscii c && isAlphaNum c

genValidId :: Gen (String, Id)
genValidId = do
  f <- arbitrary `suchThat` isAsciiAlpha
  r <- listOf (arbitrary `suchThat` isAsciiAlphaNum)
  let str = f : r
  if str `elem` ["not", "if", "then", "else", "fi", "while", "do", "done"]
    then genValidId
    else return (str, fromJust $ mkId f r)

genInvalidId :: Gen String
genInvalidId = pure <$> arbitrary `suchThat` (not . isAsciiAlpha)

genInvalidAExp :: Gen String
genInvalidAExp = pure <$> arbitrary `suchThat` (\x -> not (isDigit x) && not (isAsciiAlpha x) && x /= '(')

genValidAExp :: Gen (String, AExp)
genValidAExp = sized genAExpSized

genAExpSized :: Int -> Gen (String, AExp)
genAExpSized 0 = oneof [second Num <$> genValidNum, second Var <$> genValidId]
genAExpSized n = do
  spaces <- genSpaces
  (leftS, leftA) <- genAExpSized (n `div` 2)
  (opS, opA) <- genValidBinOp
  (rightS, rightA) <- genAExpSized (n `div` 2)
  let str = intercalate spaces ["(", leftS, opS, rightS, ")"] ++ spaces
  return (str, Op leftA opA rightA)

genInvalidExpr :: Gen String
genInvalidExpr = genInvalidAExp

genValidExpr :: Gen (String, Exp)
genValidExpr = sized genExpSized

genExpSized :: Int -> Gen (String, Exp)
genExpSized 0 = second E_AExp <$> genValidAExp
genExpSized n = oneof [genNot, genCmp, genIf, second E_AExp <$> genAExpSized n]
  where
    genNot = do
      notStr <- (++) "not " <$> genSpaces
      (eStr, eAst) <- genExpSized (n `div` 2)
      return (notStr ++ eStr, Not eAst)
    genCmp = do
      spaces <- genSpaces
      (leftS, leftA) <- genAExpSized (n `div` 2)
      (opS, opA) <- genValidCmpOp
      (rightS, rightA) <- genAExpSized (n `div` 2)
      let str = intercalate spaces [leftS, opS, rightS]
      return (str, Cmp leftA opA rightA)
    genIf = do
      spaces <- genSpaces
      (cS, cA) <- genExpSized (n `div` 3)
      (tS, tA) <- genExpSized (n `div` 3)
      (eS, eA) <- genExpSized (n `div` 3)
      let str = intercalate spaces ["if ", cS, " then ", tS, " else ", eS, " fi"]
      return (str, If cA tA eA)

genValidBinOp :: Gen (String, BinOp)
genValidBinOp = genValidOp [("+", Add), ("-", Sub), ("*", Mul), ("/", Div)]

genInvalidBinOp :: Gen String
genInvalidBinOp = genInvalidOp ["+", "-", "*", "/"]

genValidCmpOp :: Gen (String, CmpOp)
genValidCmpOp = genValidOp [("<=", Le), (">", Gt), ("==", Eq), ("!=", Neq)]

genInvalidCmpOp :: Gen String
genInvalidCmpOp = genInvalidOp ["<=", ">", "==", "!="]

genValidAssign :: Gen (String, Stmt)
genValidAssign = do
  (identStr, identAst) <- genValidId
  (expStr, expAst) <- genValidExpr
  isEq <- (++ ":=") <$> genSpaces
  return (identStr ++ isEq ++ expStr, Assign identAst expAst)

genValidWhile :: Int -> Gen (String, Stmt)
genValidWhile n = do
  whiteStr <- (++) "while " <$> genSpaces
  (eStr, eAst) <- genExpSized (n `div` 2)
  doStr <- (++) " do " <$> genSpaces
  (sStr, sAst, _) <- scale (`div` 2) genValidStmts
  doneStr <- (++) " done" <$> genSpaces
  return (whiteStr ++ eStr ++ doStr ++ sStr ++ doneStr, While eAst sAst)

genValidStmt :: Gen (String, Stmt, String)
genValidStmt = withPadding $ sized genStmtSized

genStmtSized :: Int -> Gen (String, Stmt)
genStmtSized 0 = genValidAssign
genStmtSized n = oneof [genValidAssign, genValidWhile n]

genInvalidStmt :: Gen String
genInvalidStmt = genInvalidId

genInvalidStmts :: Gen String
genInvalidStmts = genInvalidStmt

genValidStmts :: Gen (String, [Stmt], String)
genValidStmts = do
  (strList, stmtList, _) <- unzip3 <$> scale (`div` 3) (listOf1 (scale (`div` 2) genValidStmt))
  (semicolon, rest) <- first (';' :) <$> genPadding
  let input = intercalate semicolon strList
  let result = stmtList
  return (input, result, rest)

-- ==========================================
-- Properties
-- ==========================================

prop_parse_valid :: (Eq a, Show a) => ReadP a -> Gen (String, a, String) -> Property
prop_parse_valid parser generator = property $ do
  (str, ast, rest) <- generator
  return $ runReadP parser (str ++ rest) === Just (ast, rest)

prop_parse_invalid :: (Eq a, Show a) => ReadP a -> Gen String -> Property
prop_parse_invalid parser generator = property $ do
  invalidStart <- generator
  rest <- arbitrary
  return $ runReadP parser (invalidStart ++ rest) === Nothing

prop_num_valid :: Property
prop_num_valid = prop_parse_valid num (withPadding genValidNum)

prop_num_invalid :: Property
prop_num_invalid = prop_parse_invalid num genInvalidNum

prop_aexp_valid :: Property
prop_aexp_valid = prop_parse_valid aexp (withPadding genValidAExp)

prop_aexp_invalid :: Property
prop_aexp_invalid = prop_parse_invalid aexp genInvalidAExp

prop_expr_valid :: Property
prop_expr_valid = prop_parse_valid expr (scale (`div` 2) (withPadding genValidExpr))

prop_expr_invalid :: Property
prop_expr_invalid = prop_parse_invalid expr genInvalidExpr

prop_binop_valid :: Property
prop_binop_valid = prop_parse_valid binop (withPadding genValidBinOp)

prop_binop_invalid :: Property
prop_binop_invalid = prop_parse_invalid binop genInvalidBinOp

prop_cmpop_valid :: Property
prop_cmpop_valid = prop_parse_valid cmpop (withPadding genValidCmpOp)

prop_cmpop_invalid :: Property
prop_cmpop_invalid = prop_parse_invalid cmpop genInvalidCmpOp

prop_identifier_valid :: Property
prop_identifier_valid = prop_parse_valid identifier (withPadding genValidId)

prop_identifier_reject_invalid_start :: Property
prop_identifier_reject_invalid_start = prop_parse_invalid identifier genInvalidId

prop_stmt_valid :: Property
prop_stmt_valid = prop_parse_valid stmt genValidStmt

prop_stmt_invalid :: Property
prop_stmt_invalid = prop_parse_invalid stmt genInvalidStmt

prop_stmts_valid :: Property
prop_stmts_valid = prop_parse_valid stmts genValidStmts

prop_stmts_invalid :: Property
prop_stmts_invalid = prop_parse_invalid stmts genInvalidStmts

-- ==========================================
-- Test examples
-- ==========================================

prop_stmt_x_2 :: Property
prop_stmt_x_2 = property $ runReadP stmts "x :=21 Noise" === Just ([Assign (fromJust $ mkId 'x' "") (E_AExp (Num 21))], "Noise")

prop_stmt_semi :: Property
prop_stmt_semi = property $ runReadP stmts "x :=21; y:=2" === Just ([Assign (fromJust $ mkId 'x' "") (E_AExp (Num 21)), Assign (fromJust $ mkId 'y' "") (E_AExp (Num 2))], "")

prop_stmt_complex :: Property
prop_stmt_complex =
  property $
    let input = "a:=10; b:=2; res:=0; while not a<=0 do curr:=if a>5 then (a+b) else (a/b) fi; res:=(res*curr); a:=if b!=0 then (a-1) else a fi done"
        a = fromJust $ mkId 'a' ""
        b = fromJust $ mkId 'b' ""
        res = fromJust $ mkId 'r' "es"
        curr = fromJust $ mkId 'c' "urr"
        ast =
          [ Assign a (E_AExp (Num 10)),
            Assign b (E_AExp (Num 2)),
            Assign res (E_AExp (Num 0)),
            While
              (Not (Cmp (Var a) Le (Num 0)))
              [ Assign curr (If (Cmp (Var a) Gt (Num 5)) (E_AExp (Op (Var a) Add (Var b))) (E_AExp (Op (Var a) Div (Var b)))),
                Assign res (E_AExp (Op (Var res) Mul (Var curr))),
                Assign a (If (Cmp (Var b) Neq (Num 0)) (E_AExp (Op (Var a) Sub (Num 1))) (E_AExp (Var a)))
              ]
          ]
     in runReadP stmts input === Just (ast, "")