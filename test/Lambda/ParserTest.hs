{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lambda.ParserTest (parserTests) where

import Control.Applicative (Alternative (..))
import Data.Char (isAlpha, isAlphaNum, isAscii, isDigit, isSpace)
import Lambda.Parser (ReadP, digit, endOfStream, identifier, parseDigits, parseTwoChars, whiteSpace)
import Text.ParserCombinators.ReadP (char, pfail, readP_to_S, satisfy, string)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes (applicative, functor, monad)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

-- | Run parser and return the first result
runReadP :: ReadP a -> String -> Maybe (a, String)
runReadP p s = case readP_to_S p s of
  [] -> Nothing
  ((x, s') : _) -> Just (x, s') 

parserTests :: TestTree
parserTests =
  testGroup
    "Parser Suite"
    [ testCase "satisfy matches character" $ parseA "abc" @?= Just ('a', "bc"),
      testCase "satisfy does not match character" $ parseA "xbc" @?= Nothing,
      testCase "satisfy on empty string" $ parseA "" @?= Nothing,
      testCase "endOfStream matches empty string" $ runReadP endOfStream "" @?= Just ((), ""),
      testCase "fmap maps ReadP Char to ReadP String" $ runReadP ((: []) <$> satisfy (== 'a')) "abc" @?= Just ("a", "bc"),
      testCase "fmap maps with custom lambda" $ runReadP ((:) <*> (: []) <$> satisfy (== 'a')) "abc" @?= Just ("aa", "bc"),
      quickTests,
      tastyBatch
        (functor (undefined :: ReadP (Int, String, Int))),
      tastyBatch
        (applicative (undefined :: ReadP (Int, String, Int))),
      tastyBatch
        (monad (undefined :: ReadP (Int, String, Int)))
    ]
  where
    parseA = runReadP (satisfy (== 'a'))
    tastyBatch (name, tests) = testProperties name tests

quickTests :: TestTree
quickTests =
  testGroup
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
      testProperty "whiteSpace rejects non-whitespace" prop_not_whiteSpace,
      testProperty "endOfStream rejects non-empty string" prop_endOfStream_nonEmpty,
      testProperty "parseTwoChars matches both chars" prop_parseTwoChars_match_both,
      testProperty "parseTwoChars matches first char only" prop_parseTwoChars_match_first_only,
      testProperty "parseTwoChars matches second char only" prop_parseTwoChars_match_second_only,
      testProperty "parseTwoChars matches none" prop_parseTwoChars_match_none,
      testProperty "parseDigits parses sequence of digits" prop_parseDigits,
      testProperty "identifier parses valid names" prop_identifier_valid,
      testProperty "identifier rejects names with invalid start" prop_identifier_reject_invalid_start
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

prop_endOfStream_nonEmpty :: Char -> String -> Property
prop_endOfStream_nonEmpty c s =
  runReadP endOfStream (c : s) === Nothing



prop_parseTwoChars_match_both :: String -> Property
prop_parseTwoChars_match_both s =
  runReadP parseTwoChars ("ab" ++ s) === Just ("ab", s)

prop_parseTwoChars_match_first_only :: Char -> String -> Property
prop_parseTwoChars_match_first_only c s =
  c /= 'b' ==> runReadP parseTwoChars ('a' : c : s) === Nothing

prop_parseTwoChars_match_second_only :: Char -> String -> Property
prop_parseTwoChars_match_second_only c s =
  c /= 'a' ==> runReadP parseTwoChars (c : 'b' : s) === Nothing

prop_parseTwoChars_match_none :: Char -> Char -> String -> Property
prop_parseTwoChars_match_none c1 c2 s =
  c1 /= 'a' && c2 /= 'b' ==> runReadP parseTwoChars (c1 : c2 : s) === Nothing

prop_parseDigits :: NonNegative Int -> Char -> String -> Property
prop_parseDigits (NonNegative n) c s = not (isDigit c) ==> 
  runReadP parseDigits (show n ++ (c:s)) === Just (n, c:s)

(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
f &&& g = \x -> f x && g x
infixr 3 &&&

genInputStartingWithId :: Gen (String, String)
genInputStartingWithId = do
  idFirst <- arbitrary `suchThat` (isAscii &&& isAlpha)
  idRest <- listOf $ arbitrary `suchThat` (isAscii &&& isAlphaNum)
  separator <- arbitrary `suchThat` (\c -> not (isAscii c && isAlphaNum c))
  rest <- arbitrary
  return (idFirst : idRest, separator : rest)

prop_identifier_valid :: Property
prop_identifier_valid = 
  forAll genInputStartingWithId $ \(idStr, rest) ->
    runReadP identifier (idStr ++ rest) === Just (idStr, rest)

genInvalidStartChar :: Gen Char
genInvalidStartChar = frequency
  [ (1, elements ['0'..'9'])
  , (3, arbitrary `suchThat` (\c -> not (isAscii c && isAlpha c)))
  ]

prop_identifier_reject_invalid_start :: String -> Property
prop_identifier_reject_invalid_start s = 
  forAll genInvalidStartChar $ \c ->
    classify (isDigit c) "first char is digit" $
      runReadP identifier (c : s) === Nothing