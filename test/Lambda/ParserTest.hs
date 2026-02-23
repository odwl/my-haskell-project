module Lambda.ParserTest (parserTests) where

import Parser (Parser (..), satisfy)
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
      quickCheckAnagramTests
    ]
  where
    parseA = runParser (satisfy (== 'a'))

runParser :: Parser a -> String -> Maybe (a, String)
runParser (Parser p) = p

quickCheckAnagramTests :: TestTree
quickCheckAnagramTests =
  testGroup
    "QuickCheck"
    [ testProperty "satisfy strictly matches character" prop_satisfiesMatchingChar
    ]

prop_satisfiesMatchingChar :: Char -> String -> Property
prop_satisfiesMatchingChar c s =
  runParser (satisfy (== c)) (c : s) === Just (c, s)
