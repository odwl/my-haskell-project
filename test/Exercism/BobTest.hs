{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Exercism.BobTest (bobTests, main) where

import Data.Char (isDigit, isSpace)
import qualified Data.Text as T
import Exercism.Bob (ResponseType (..), responseFor, responseTxt)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    Property,
    elements,
    forAll,
    listOf,
    testProperty,
    (===),
  )

bobTests :: TestTree
bobTests =
  testGroup
    "Bob"
    [ quickCheckBobTests,
      exampleTests
    ]

-- ==========================================
-- QuickCheck Properties
-- ==========================================

quickCheckBobTests :: TestTree
quickCheckBobTests =
  testGroup
    "QuickCheck"
    [ testProperty "silence always gets the same response" prop_silence,
      testProperty "yelling always gets a yelling response" prop_yelling,
      testProperty "questions get specific responses" prop_question_finished_by,
      testProperty "not a question gets specific responses" prop_not_a_question,
      testProperty "not silence gets specific responses" prop_not_silence
    ]

prop_silence :: Property
prop_silence =
  forAll genNoise ((=== responseTxt Fine) . responseFor)

prop_not_silence :: Property
prop_not_silence =
  forAll genNotSilence ((/= responseTxt Fine) . responseFor)

prop_yelling :: Property
prop_yelling =
  forAll genYelling ((`elem` (responseTxt <$> [Whoa, CalmDown])) . responseFor)

-- Verifies that any input ending with a question mark (followed by optional whitespace)
-- is treated as a question by Bob.
prop_question_finished_by :: Property
prop_question_finished_by = forAll genAQuestion $ \input -> responseFor input `elem` (responseTxt <$> [CalmDown, Sure])

prop_not_a_question :: Property
prop_not_a_question = forAll genNotAQuestion $ \input -> responseFor input `elem` (responseTxt <$> [Whoa, Whatever])

-- ==========================================
-- QuickCheck Helpers
-- ==========================================

-- Examples: ' ', '\t', '\n'
noiseChars :: [Char]
noiseChars = filter isSpace ['\0' .. '\127']

-- Examples: 'a', 'Z', '1', '!'
nonSpaceChars :: [Char]
nonSpaceChars = filter (\c -> not (isSpace c) && c /= '?') ['\0' .. '\127']

-- Examples: ' ', '\t', '1', '9', '@', '.'
baseNoiseChars :: [Char]
baseNoiseChars = filter isBaseNoiseChar ['\0' .. '\127']
  where
    isBaseNoiseChar c = isSpace c || isDigit c || c `elem` ("!@#$%^&*,." :: String)

-- ==========================================
-- QuickCheck Generators
-- ==========================================

-- Example: "How are you?  "
genAQuestion :: Gen T.Text
genAQuestion = do
  s <- T.pack <$> arbitrary
  noise <- genNoise
  pure (s <> "?" <> noise)

-- Example: "  \t"
genNoise :: Gen T.Text
genNoise = T.pack <$> listOf (elements noiseChars)

-- Example: "Hello!"
genNotAQuestion :: Gen T.Text
genNotAQuestion = do
  s <- T.pack <$> arbitrary
  nonSpaceChar <- genNonSpaceChar
  noise <- genNoise
  pure (s <> nonSpaceChar <> noise)

-- Example: 'A'
genNonSpaceChar :: Gen T.Text
genNonSpaceChar = T.singleton <$> elements nonSpaceChars

-- Example: " a "
genNotSilence :: Gen T.Text
genNotSilence = do
  n1 <- genNoise
  c <- genNonSpaceChar
  n2 <- genNoise
  pure (n1 <> c <> n2)

-- Example: "1, 2, 3 GO!  "
genYelling :: Gen T.Text
genYelling = do
  baseNoise <- T.pack <$> listOf (elements baseNoiseChars)
  upperCaseLetter <- elements ['A' .. 'Z']
  moreUpperCase <- T.pack <$> listOf (elements ['A' .. 'Z'])
  pure $ baseNoise <> T.singleton upperCaseLetter <> moreUpperCase <> baseNoise

-- ==========================================
-- Example Tests
-- ==========================================

exampleTests :: TestTree
exampleTests =
  testGroup
    "Examples"
    [ testCase explanation $ responseFor input @?= expected
      | Case {..} <- cases
    ]

data Case = Case {explanation :: String, input :: T.Text, expected :: T.Text}

cases :: [Case]
cases =
  [ Case "stating something" "Tom-ay-to, tom-aaaah-to." (responseTxt Whatever),
    Case "shouting" "WATCH OUT!" (responseTxt Whoa),
    Case "shouting gibberish" "FCECDFCAAB" (responseTxt Whoa),
    Case "asking a question" "Does this cryogenic chamber make me look fat?" (responseTxt Sure),
    Case "asking a numeric question" "You are, what, like 15?" (responseTxt Sure),
    Case "asking a question in yelling" "TO THE WEST?" (responseTxt CalmDown),
    Case "talking forcefully" "Let's go make out behind the gym!" (responseTxt Whatever),
    Case "silence" "" (responseTxt Fine),
    Case "prolonged silence" "   " (responseTxt Fine),
    Case "only numbers" "1, 2, 3" (responseTxt Whatever),
    Case "shouting numbers" "1, 2, 3 GO!" (responseTxt Whoa),
    Case "question with no letters" "4?" (responseTxt Sure)
  ]

main :: IO ()
main = defaultMain bobTests
