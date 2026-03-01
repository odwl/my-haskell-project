{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lambda.ParserTest (parserTests) where

import Control.Applicative (Alternative (..))
import Data.Char (isAlpha, isAlphaNum, isAscii, isDigit, isSpace, isPunctuation, isSymbol, isUpper, isLower)
import Data.List (intercalate)
import Lambda.Parser (AExp (..), Exp (..), Id, mkId, ReadP, Stmt (..), Stmts (..), aexp, digit, identifier, num, stmt, stmts, whiteSpace)
import Data.Maybe (fromJust)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes (applicative, functor, monad)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.ParserCombinators.ReadP (char, pfail, readP_to_S, satisfy, string)
import Prelude hiding (exp)

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
        testProperty "Alternative is left-biased" prop_alternative_left_bias,
        testProperty "digit matches digits" prop_digit,
        testProperty "digit rejects non-digits" prop_not_digit,
        testProperty "whiteSpace matches whitespace" prop_whiteSpace,
        testProperty "whiteSpace rejects non-whitespace" prop_not_whiteSpace
      ],
    testGroup
      "AST Parsing Properties"
      [ testProperty "num parses sequence of digits" prop_num,
        testProperty "num rejects invalid sequence" prop_num_invalid,
        testProperty "identifier parses valid names" prop_identifier_valid,
        testProperty "identifier rejects names with invalid start" prop_identifier_reject_invalid_start,
        testProperty "aexp parses numbers" prop_aexp_num,
        testProperty "aexp rejects invalid sequence" prop_aexp_invalid,
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

prop_digit :: String -> Property
prop_digit s = forAll (elements ['0' .. '9']) $ \c ->
  runReadP digit (c : s) === Just (c, s)

prop_not_digit :: String -> Property
prop_not_digit s = forAll (arbitrary `suchThat` (not . isDigit)) $ \c ->
  runReadP digit (c : s) === Nothing

prop_whiteSpace :: String -> Property
prop_whiteSpace s = forAll (elements " \t\n\r\f\v") $ \c ->
  runReadP whiteSpace (c : s) === Just (c, s)

prop_not_whiteSpace :: String -> Property
prop_not_whiteSpace s = forAll (arbitrary `suchThat` (not . isSpace)) $ \c ->
  runReadP whiteSpace (c : s) === Nothing

-- ==========================================
-- Test num and aexp
-- ==========================================

prop_num :: Property
prop_num = property $ do
  (nStr, n, s) <- genNum
  return $ runReadP num (nStr ++ s) === Just (n, s)

prop_num_invalid :: Property
prop_num_invalid = property $ do
  s <- genNotNum
  return $ runReadP num s === Nothing

prop_aexp_num :: Property
prop_aexp_num = property $ do
  (nStr, n, s) <- genNum
  return $ runReadP aexp (nStr ++ s) === Just (Num n, s)

prop_aexp_invalid :: Property
prop_aexp_invalid = property $ do
  s <- genNotNum
  return $ runReadP aexp s === Nothing

genSpaces :: Gen String
genSpaces = elements $ "" : map pure " \t\n\r\f\v"

genSeparator :: Gen Char
genSeparator = arbitrary `suchThat` (\c -> isPunctuation c || isSymbol c)

-- | Generates a valid numeric value followed by trailing whitespace and noise.
-- Returns a tuple containing:
-- 1. The full unparsed literal string including the number and trailing spaces
-- 2. The expected generated Int value
-- 3. A random trailing string guaranteed to start with a non-digit, non-space character
genNum :: Gen (String, Int, String)
genNum = do
  n <- arbitrary `suchThat` (>= 0)
  spaces <- genSpaces
  separator <- genSeparator
  rest <- arbitrary
  return (show n ++ spaces, n, separator : rest)

genNotNum :: Gen String
genNotNum = do
  c <- arbitrary `suchThat` (not . isDigit)
  s <- arbitrary
  return (c : s)

-- ==========================================
-- Test identifier
-- ==========================================

prop_identifier_valid :: Property
prop_identifier_valid = property $ do
  (input, ident, rest) <- genValidIdentifier
  return $ runReadP identifier (input ++ rest) === Just (ident, rest)

prop_identifier_reject_invalid_start :: Property
prop_identifier_reject_invalid_start = property $ do 
  invalidStart <- genInvalidStartChar
  rest <- arbitrary
  return $ classify (isDigit invalidStart) "first char is digit" $
           classify (isUpper invalidStart) "first char is upper" $
           classify (isLower invalidStart) "first char is lower" $
           classify (isPunctuation invalidStart) "first char is punctuation" $
           classify (isSymbol invalidStart) "first char is symbol" $
           classify (isSpace invalidStart) "first char is space" $
           classify (isAscii invalidStart) "first char is ascii" $
           classify (isAlphaNum invalidStart) "first char is alphanumeric" $
    runReadP identifier (invalidStart : rest) === Nothing

-- | Generates a valid identifier string followed by trailing noise.
-- Returns a tuple containing:
-- 1. The full unparsed literal string including the identifier and trailing spaces
-- 2. The expected generated Id AST node
-- 3. A random trailing string guaranteed to start with a non-alphanumeric separator character
genValidIdentifier :: Gen (String, Id, String)
genValidIdentifier = do
  idFirst <- arbitrary `suchThat` (liftA2 (&&) isAscii isAlpha)
  idRest <- listOf $ arbitrary `suchThat` (liftA2 (&&) isAscii isAlphaNum)
  spaces <- genSpaces
  separator <- genSeparator
  rest <- arbitrary
  return (idFirst : idRest ++ spaces, fromJust $ mkId idFirst idRest, separator : rest)

genInvalidStartChar :: Gen Char
genInvalidStartChar =
  frequency
    [ (1, elements ['0' .. '9']),
      (3, arbitrary `suchThat` (\c -> not (isAscii c && isAlpha c)))
    ]

-- ==========================================
-- Test stmt
-- ==========================================

prop_stmt_valid :: Property
prop_stmt_valid =
  forAll genValidStmt $ \(stmtStr, stmtAst, rest) ->
    runReadP stmt (stmtStr ++ rest) === Just (stmtAst, rest)

-- | Generates a valid statement.
-- Returns a tuple containing:
-- 1. The literal string representation of the statement (e.g. "x := \t 21  ")
-- 2. The expected AST (e.g. Assign "x" (E_AExp (Num 21)))
-- 3. A random trailing "rest" string that should be left unparsed
genValidStmt :: Gen (String, Stmt, String)
genValidStmt = do
  (identStr, identAst, _) <- genValidIdentifier
  (nStr, n, _) <- genNum
  spaces <- genSpaces
  separator <- genSeparator
  rest <- arbitrary 
  let stmtStr = identStr ++ ":=" ++ spaces ++ nStr
  let stmtAst = Assign identAst (E_AExp (Num n))
  return (stmtStr, stmtAst, separator : rest)

-- | Generates a valid sequence of statements separated by semicolons.
-- Returns a tuple containing:
-- 1. The literal string representation of the statements (e.g. "x := \t 1;y := 2")
-- 2. The expected Stmts AST (e.g. Seq (Assign "x" (E_AExp (Num 1))) (Single (Assign "y" (E_AExp (Num 2)))))
-- 3. A random trailing "rest" string that should be left unparsed
genValidStmts :: Gen (String, Stmts, String)
genValidStmts = do
  (strList, stmtList, restList) <- unzip3 <$> listOf1 genValidStmt
  semicolon <- (';' :) <$> genSpaces
  let input = intercalate semicolon strList
  let buildStmts [] = error "Unreachable"
      buildStmts [s] = Single s
      buildStmts (s : ss) = Seq s (buildStmts ss)
  let result = buildStmts stmtList
  let finalRest = last restList
  return (input, result, finalRest)

prop_stmts_valid :: Property
prop_stmts_valid = property $ do
  (stmtsStr, stmtsAst, rest) <- genValidStmts
  return $ runReadP stmts (stmtsStr ++ rest) === Just (stmtsAst, rest)

-- ==========================================
-- Test examples
-- ==========================================

prop_stmt_x_2 :: Property
prop_stmt_x_2 = property $ runReadP stmts "x :=21 Noise" === Just (Single (Assign (fromJust $ mkId 'x' "") (E_AExp (Num 21))), "Noise")

prop_stmt_semi :: Property
prop_stmt_semi = property $ runReadP stmts "x :=21; y:=2" === Just (Seq (Assign (fromJust $ mkId 'x' "") (E_AExp (Num 21))) (Single (Assign (fromJust $ mkId 'y' "") (E_AExp (Num 2)))), "")


-- Just (Id 'y' "yO9","?#\31796") /= 
-- Just (Id 'y' "yO9","\r?#\31796")

-- Just (Id 'K' "15uFLh5O4kUBe9OPTb7SRJ34U7W","D\SUB\1061255|\r\21395o\nD\6364\f\33097+rsp\CAN>") /= 
-- Just (Id 'K' "15uFLh5O4kUBe9OPTb7SRJ34U7W","\rD\SUB\1061255|\r\21395o\nD\6364\f\33097+rsp\CAN>")