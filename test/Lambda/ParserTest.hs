{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lambda.ParserTest (parserTests) where

import Control.Applicative (Alternative (..))
import Data.Char (isDigit, isSpace)
import Lambda.Parser (Parser (..), digit, endOfStream, parseChar, parseString, parseTwoChars, parserInt, satisfy, whiteSpace)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes (applicative, functor, monad)
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
      tastyBatch
        (functor (undefined :: Parser (Int, String, Int))),
      tastyBatch
        (applicative (undefined :: Parser (Int, String, Int))),
      tastyBatch
        (monad (undefined :: Parser (Int, String, Int)))
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
      testProperty "parseChar matches character" prop_parseChar,
      testProperty "parseChar rejects non-matching character" prop_parseChar_nothing,
      testProperty "parseString matches specific string" prop_parseString,
      testProperty "Alternative <|> tries second parser on failure" prop_alternative_choice,
      testProperty "Alternative empty always fails" prop_alternative_empty,
      testProperty "Alternative Law: Left Identity" prop_alternative_left_identity,
      testProperty "Alternative Law: Right Identity" prop_alternative_right_identity,
      testProperty "Alternative is left-biased" prop_alternative_left_bias,
      testProperty "digit matches digits" prop_digit,
      testProperty "digit rejects non-digits" prop_not_digit,
      testProperty "whiteSpace matches whitespace" prop_whiteSpace,
      testProperty "whiteSpace rejects non-whitespace" prop_not_whiteSpace,
      testProperty "endOfStream rejects non-empty string" prop_endOfStream_nonEmpty,
      testProperty "parserInt parses a single digit correctly" prop_parserInt_digit,
      testProperty "parserInt rejects non-digit structures" prop_parserInt_not_digit,
      testProperty "parseTwoChars matches both chars" prop_parseTwoChars_match_both,
      testProperty "parseTwoChars matches first char only" prop_parseTwoChars_match_first_only,
      testProperty "parseTwoChars matches second char only" prop_parseTwoChars_match_second_only,
      testProperty "parseTwoChars matches none" prop_parseTwoChars_match_none
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

prop_parseChar :: Char -> String -> Property
prop_parseChar c s =
  runParser (parseChar c) (c : s) === Just (c, s)

prop_parseChar_nothing :: Char -> Char -> String -> Property
prop_parseChar_nothing c1 c2 s =
  c1 /= c2 ==> runParser (parseChar c1) (c2 : s) === Nothing

prop_parseString :: String -> String -> Property
prop_parseString target rest =
  runParser (parseString target) (target ++ rest) === Just (target, rest)

prop_alternative_choice :: String -> Property
prop_alternative_choice s =
  runParser (parseChar 'a' <|> parseChar 'b') ("b" ++ s) === Just ('b', s)

prop_alternative_empty :: String -> Property
prop_alternative_empty s =
  runParser (empty :: Parser Int) s === Nothing

prop_alternative_left_identity :: Parser Int -> String -> Property
prop_alternative_left_identity p s =
  runParser (empty <|> p) s === runParser p s

prop_alternative_right_identity :: Parser Int -> String -> Property
prop_alternative_right_identity p s =
  runParser (p <|> empty) s === runParser p s

prop_alternative_left_bias :: String -> Property
prop_alternative_left_bias s =
  runParser (parseString "a" <|> parseString "ab") ("ab" ++ s) === Just ("a", "b" ++ s)

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

prop_parserInt_digit :: String -> Property
prop_parserInt_digit s = forAll (elements ['0' .. '9']) $ \c ->
  runParser parserInt (c : s) === Just (read [c], s)

prop_parserInt_not_digit :: Char -> String -> Property
prop_parserInt_not_digit c s =
  not (isDigit c) ==> runParser parserInt (c : s) === Nothing

prop_parseTwoChars_match_both :: String -> Property
prop_parseTwoChars_match_both s =
  runParser parseTwoChars ("ab" ++ s) === Just ("ab", s)

prop_parseTwoChars_match_first_only :: Char -> String -> Property
prop_parseTwoChars_match_first_only c s =
  c /= 'b' ==> runParser parseTwoChars ('a' : c : s) === Nothing

prop_parseTwoChars_match_second_only :: Char -> String -> Property
prop_parseTwoChars_match_second_only c s =
  c /= 'a' ==> runParser parseTwoChars (c : 'b' : s) === Nothing

prop_parseTwoChars_match_none :: Char -> Char -> String -> Property
prop_parseTwoChars_match_none c1 c2 s =
  c1 /= 'a' && c2 /= 'b' ==> runParser parseTwoChars (c1 : c2 : s) === Nothing