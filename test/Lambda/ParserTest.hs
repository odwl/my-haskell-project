{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lambda.ParserTest (parserTests) where

import Data.Char (isDigit, isSpace)
import Parser (Parser (..), digit, endOfStream, satisfy, term1, whiteSpace)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes (functor)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

parserTests :: TestTree
parserTests =
  testGroup
    "Parser Suite"
    [ testCase "satisfy matches character" $ parseA "abc" @?= Just ('a', "bc"),
      testCase "satisfy does not match character" $ parseA "xbc" @?= Nothing,
      testCase "satisfy on empty string" $ parseA "" @?= Nothing,
      testCase "endOfStream matches empty string" $ runParser endOfStream "" @?= Just ((), ""),
      testCase "fmap maps Parser Char to Parser String" $ runParser ((: []) <$> satisfy (== 'a')) "abc" @?= Just ("a", "bc"),
      testCase "fmap maps with custom lambda" $ runParser ((:) <*> (: []) <$> satisfy (== 'a')) "abc" @?= Just ("aa", "bc"),
      quickTests,
      tastyBatch (functor (undefined :: Parser (Int, String, Int)))
    ]
  where
    parseA = runParser (satisfy (== 'a'))
    tastyBatch (name, tests) = testProperties name tests

runParser :: Parser a -> String -> Maybe (a, String)
runParser (Parser p) = p

quickTests :: TestTree
quickTests =
  testGroup
    "QuickCheck"
    [ testProperty "satisfy strictly matches character" prop_satisfiesMatchingChar,
      testProperty "satisfy rejects non-matching character" prop_nonMatchingChar,
      testProperty "satisfy rejects empty string" prop_emptyString,
      testProperty "term1 matches character" prop_term1,
      testProperty "term1 rejects non-matching character" prop_term1_nothing,
      testProperty "digit matches digits" prop_digit,
      testProperty "digit rejects non-digits" prop_not_digit,
      testProperty "whiteSpace matches whitespace" prop_whiteSpace,
      testProperty "whiteSpace rejects non-whitespace" prop_not_whiteSpace,
      testProperty "endOfStream rejects non-empty string" prop_endOfStream_nonEmpty
    ]

instance (Arbitrary a) => Arbitrary (Parser a) where
  arbitrary = do
    b <- arbitrary
    -- Generate a parser that either succeeds with a value and arbitrary leftover string, or fails
    elements
      [ Parser (const Nothing),
        Parser (\s -> Just (b, s))
      ]

instance (Show a) => Show (Parser a) where
  show _ = "<Parser>"

instance (Eq a, Show a) => EqProp (Parser a) where
  p1 =-= p2 =
    property $ \s -> runParser p1 s === runParser p2 s

prop_satisfiesMatchingChar :: Char -> String -> Property
prop_satisfiesMatchingChar c s =
  runParser (satisfy (== c)) (c : s) === Just (c, s)

prop_nonMatchingChar :: Char -> Char -> String -> Property
prop_nonMatchingChar c1 c2 s =
  c1 /= c2 ==> runParser (satisfy (== c1)) (c2 : s) === Nothing

prop_emptyString :: Char -> Property
prop_emptyString c =
  runParser (satisfy (== c)) "" === Nothing

prop_term1 :: Char -> String -> Property
prop_term1 c s =
  runParser (term1 c) (c : s) === Just (c, s)

prop_term1_nothing :: Char -> Char -> String -> Property
prop_term1_nothing c1 c2 s =
  c1 /= c2 ==> runParser (term1 c1) (c2 : s) === Nothing

prop_digit :: String -> Property
prop_digit s = forAll (elements ['0' .. '9']) $ \c ->
  runParser digit (c : s) === Just (c, s)

prop_not_digit :: String -> Property
prop_not_digit s = forAll (arbitrary `suchThat` (not . isDigit)) $ \c ->
  runParser digit (c : s) === Nothing

prop_whiteSpace :: String -> Property
prop_whiteSpace s = forAll (elements " \t\n\r\f\v") $ \c ->
  runParser whiteSpace (c : s) === Just (c, s)

prop_not_whiteSpace :: String -> Property
prop_not_whiteSpace s = forAll (arbitrary `suchThat` (not . isSpace)) $ \c ->
  runParser whiteSpace (c : s) === Nothing

prop_endOfStream_nonEmpty :: Char -> String -> Property
prop_endOfStream_nonEmpty c s =
  runParser endOfStream (c : s) === Nothing

-- -- strToInt x = case (readMaybe x) of
-- -- Nothing -> error "Cannot convert to Int"
-- -- Just i -> i