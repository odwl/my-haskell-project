{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lambda.ParserTest (parserTests) where

import Control.Applicative (Alternative (..))
import Data.Char (isAlpha, isAlphaNum, isAscii, isDigit, isSpace, isPunctuation, isSymbol)
import Data.List (intercalate, isPrefixOf)
import Lambda.Parser (AExp (..), Exp (..), Id, mkId, ReadP, Stmt (..), Stmts (..), aexp, expr, identifier, num, stmt, stmts, binop, BinOp (..), cmpop, CmpOp (..))
import Data.Maybe (fromJust)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes (applicative, functor, monad)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.Bifunctor (first)
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
        testProperty "expr parses valid aexp" prop_expr_valid,
        testProperty "expr rejects invalid sequence" prop_expr_invalid,
        testProperty "binop parses valid operators" prop_binop_valid,
        testProperty "binop rejects invalid operators" prop_binop_invalid,
        testProperty "cmpop parses valid operators" prop_cmpop_valid,
        testProperty "cmpop rejects invalid operators" prop_cmpop_invalid,
        testProperty "stmt matches valid assignment" prop_stmt_valid,
        testProperty "stmts matches valid sequence of assignments" prop_stmts_valid
      ],
    testGroup
      "Test Examples"
      [ testProperty "stmts parses x:=2" prop_stmt_x_2,
        testProperty "stmt parses x:=2;y=1" prop_stmt_semi
        -- testCase "parses complex program" $
        --       let input = "a:=10; b:=2; res:=0; while not a<=0 do curr:=if a>5 then (a+b) else (a/b) fi; res:=(res*curr); a:=if b!=0 then (a-1) else a fi done"
        --           -- TODO: Replace `undefined` with your top-level parser (e.g., `parseStmts` or `stmts`)
        --           parser = undefined :: ReadP [Stmt]
        --        in runReadP parser input @?= Just ([], "") -- TODO: Replace `[]` with the full AST
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

-- | Generates leading whitespace and trailing noise separated by a valid separator
-- Returns a tuple containing: (spaces, separator : rest)
genPadding :: Gen (String, String)
genPadding = do
  spaces <- genSpaces
  sep <- arbitrary `suchThat` (\c -> isPunctuation c || isSymbol c)
  rest <- arbitrary
  return (spaces, sep : rest)

-- | Generates a valid numeric value followed by trailing whitespace and noise.
-- Returns a tuple containing:
-- 1. The full unparsed literal string including the number and trailing spaces
-- 2. The expected generated Int value
-- 3. A random trailing string guaranteed to start with a non-digit, non-space character
genValidNum :: Gen (String, Int, String)
genValidNum = do
  (nStr, n) <- (\x -> (show x, x)) <$> arbitrary `suchThat` (>= 0)
  (spaces, rest) <- genPadding
  return (nStr ++ spaces, n, rest)

genInvalidNum :: Gen String
genInvalidNum = do
  c <- arbitrary `suchThat` (not . isDigit)
  s <- arbitrary
  return (c : s)

genOp :: [(String, a)] -> Gen (String, a, String)
genOp ops = do
  (opStr, opAst) <- elements ops
  (spaces, rest) <- genPadding
  return (opStr ++ spaces, opAst, rest)

genInvalidOp :: [String] -> Gen String
genInvalidOp invalidPrefixes = do
  s <- arbitrary `suchThat` \str -> 
    not (null str) && not (isSpace (head str)) && not (any (`isPrefixOf` str) invalidPrefixes)
  return s

-- | Generates a valid identifier string followed by trailing noise.
-- Returns a tuple containing:
-- 1. The full unparsed literal string including the identifier and trailing spaces
-- 2. The expected generated Id AST node
-- 3. A random trailing string guaranteed to start with a non-alphanumeric separator character
genValidId :: Gen (String, Id, String)
genValidId = do
  idFirst <- arbitrary `suchThat` (liftA2 (&&) isAscii isAlpha)
  idRest <- listOf $ arbitrary `suchThat` (liftA2 (&&) isAscii isAlphaNum)
  (spaces, rest) <- genPadding
  return (idFirst : idRest ++ spaces, fromJust $ mkId idFirst idRest, rest)

genInvalidStartChar :: Gen Char
genInvalidStartChar =
  frequency
    [ (1, elements ['0' .. '9']),
      (3, arbitrary `suchThat` (\c -> not (isAscii c && isAlpha c)))
    ]

genInvalidId :: Gen String
genInvalidId = do
  c <- genInvalidStartChar
  rest <- arbitrary
  return (c : rest)

genInvalidAExp :: Gen String
genInvalidAExp = do
  c <- arbitrary `suchThat` (\x -> not (isDigit x) && not (isAscii x && isAlpha x))
  s <- arbitrary
  return (c : s)

genValidAExp :: Gen (String, AExp, String)
genValidAExp = oneof
  [ first Num <$> genValidNum
  , first Var <$> genValidId
  ]

genInvalidExpr :: Gen String
genInvalidExpr = genInvalidAExp

genValidExpr :: Gen (String, Exp, String)
genValidExpr = first E_AExp <$> genValidAExp

genValidBinOp :: Gen (String, BinOp, String)
genValidBinOp = genOp [ ("+", Add), ("-", Sub), ("*", Mul), ("/", Div) ]

genInvalidBinOp :: Gen String
genInvalidBinOp = genInvalidOp ["+", "-", "*", "/"]

genValidCmpOp :: Gen (String, CmpOp, String)
genValidCmpOp = genOp [ ("<=", Le), (">", Gt), ("==", Eq), ("!=", Neq) ]

genInvalidCmpOp :: Gen String
genInvalidCmpOp = genInvalidOp ["<=", ">", "==", "!="]

-- | Generates a valid statement.
-- Returns a tuple containing:
-- 1. The literal string representation of the statement (e.g. "x := \t 21  ")
-- 2. The expected AST (e.g. Assign "x" (E_AExp (Num 21)))
-- 3. A random trailing "rest" string that should be left unparsed
genValidStmt :: Gen (String, Stmt, String)
genValidStmt = do
  (identStr, identAst, _) <- genValidId
  (expStr, expAst, _) <- genValidExpr
  (spaces, rest) <- genPadding
  let stmtStr = identStr ++ ":=" ++ spaces ++ expStr
  let stmtAst = Assign identAst expAst
  return (stmtStr, stmtAst, rest)

-- | Generates a valid sequence of statements separated by semicolons.
-- Returns a tuple containing:
-- 1. The literal string representation of the statements (e.g. "x := \t 1;y := 2")
-- 2. The expected Stmts AST (e.g. Seq (Assign "x" (E_AExp (Num 1))) (Single (Assign "y" (E_AExp (Num 2)))))
-- 3. A random trailing "rest" string that should be left unparsed
genValidStmts :: Gen (String, Stmts, String)
genValidStmts = do
  (strList, stmtList, _) <- unzip3 <$> listOf1 genValidStmt
  (semicolon, rest) <- first (';':) <$> genPadding
  let input = intercalate semicolon strList
  let buildStmts [] = error "Unreachable"
      buildStmts [s] = Single s
      buildStmts (s : ss) = Seq s (buildStmts ss)
  let result = buildStmts stmtList
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
  return $ runReadP parser invalidStart === Nothing

prop_num_valid :: Property
prop_num_valid = prop_parse_valid num genValidNum

prop_num_invalid :: Property
prop_num_invalid = prop_parse_invalid num genInvalidNum

prop_aexp_valid :: Property
prop_aexp_valid = prop_parse_valid aexp genValidAExp

prop_aexp_invalid :: Property
prop_aexp_invalid = prop_parse_invalid aexp genInvalidAExp

prop_expr_valid :: Property
prop_expr_valid = prop_parse_valid expr genValidExpr

prop_expr_invalid :: Property
prop_expr_invalid = prop_parse_invalid expr genInvalidExpr

prop_binop_valid :: Property
prop_binop_valid = prop_parse_valid binop genValidBinOp

prop_binop_invalid :: Property
prop_binop_invalid = prop_parse_invalid binop genInvalidBinOp

prop_cmpop_valid :: Property
prop_cmpop_valid = prop_parse_valid cmpop genValidCmpOp

prop_cmpop_invalid :: Property
prop_cmpop_invalid = prop_parse_invalid cmpop genInvalidCmpOp

prop_identifier_valid :: Property
prop_identifier_valid = prop_parse_valid identifier genValidId

prop_identifier_reject_invalid_start :: Property
prop_identifier_reject_invalid_start = prop_parse_invalid identifier genInvalidId

prop_stmt_valid :: Property
prop_stmt_valid = prop_parse_valid stmt genValidStmt

prop_stmts_valid :: Property
prop_stmts_valid = prop_parse_valid stmts genValidStmts

-- ==========================================
-- Test examples
-- ==========================================

prop_stmt_x_2 :: Property
prop_stmt_x_2 = property $ runReadP stmts "x :=21 Noise" === Just (Single (Assign (fromJust $ mkId 'x' "") (E_AExp (Num 21))), "Noise")

prop_stmt_semi :: Property
prop_stmt_semi = property $ runReadP stmts "x :=21; y:=2" === Just (Seq (Assign (fromJust $ mkId 'x' "") (E_AExp (Num 21))) (Single (Assign (fromJust $ mkId 'y' "") (E_AExp (Num 2)))), "")
