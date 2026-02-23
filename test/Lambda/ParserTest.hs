module Lambda.ParserTest (parserTests) where

import Parser (Parser (..), satisfy, term1)
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
      quickTests
    ]
  where
    parseA = runParser (satisfy (== 'a'))

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
      testProperty "term1 rejects non-matching character" prop_term1_nothing
    ]

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
