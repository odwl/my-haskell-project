{-# LANGUAGE RecordWildCards #-}

module Exercism.BobTest (bobTests, main) where

import Data.Char (isSpace)
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
  forAll genYelling ((flip elem (responseTxt <$> [Whoa, CalmDown])) . responseFor)

-- Verifies that any input ending with a question mark (followed by optional whitespace)
-- is treated as a question by Bob.
prop_question_finished_by :: Property
prop_question_finished_by = forAll genAQuestion $ \input -> responseFor input `elem` (responseTxt <$> [CalmDown, Sure])

prop_not_a_question :: Property
prop_not_a_question = forAll genNotAQuestion $ \input -> responseFor input `elem` (responseTxt <$> [Whoa, Whatever])

-- Example: "How are you?  "
genAQuestion :: Gen T.Text
genAQuestion = do
  s <- T.pack <$> (arbitrary :: Gen String)
  noise <- genNoise
  pure (s <> T.pack "?" <> noise)

-- Example: "  \t"
genNoise :: Gen T.Text
genNoise = T.pack <$> listOf (elements (filter isSpace ['\0' .. '\127']))

-- Example: "Hello!"
genNotAQuestion :: Gen T.Text
genNotAQuestion = do
  s <- T.pack <$> (arbitrary :: Gen String)
  nonSpaceChar <- genNonSpaceChar
  noise <- genNoise
  pure (s <> T.pack [nonSpaceChar] <> noise)

-- Example: 'A'
genNonSpaceChar :: Gen Char
genNonSpaceChar = elements $ filter (\c -> not (isSpace c) && c /= '?') ['\0' .. '\127']

-- Example: " a "
genNotSilence :: Gen T.Text
genNotSilence = do
  n1 <- genNoise
  c <- genNonSpaceChar
  n2 <- genNoise
  pure (n1 <> T.pack [c] <> n2)

-- Example: "1, 2, 3 GO!  "
genYelling :: Gen T.Text
genYelling = do
  baseNoise <- listOf (elements (filter (\c -> isSpace c || (c >= '0' && c <= '9') || c `elem` ("!@#$%^&*,." :: String)) ['\0' .. '\127']))
  upperCaseLetter <- elements ['A' .. 'Z']
  moreUpperCase <- listOf (elements ['A' .. 'Z'])
  pure $ T.pack $ baseNoise ++ [upperCaseLetter] ++ moreUpperCase ++ baseNoise

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
  [ Case "stating something" (T.pack "Tom-ay-to, tom-aaaah-to.") (responseTxt Whatever),
    Case "shouting" (T.pack "WATCH OUT!") (responseTxt Whoa),
    Case "shouting gibberish" (T.pack "FCECDFCAAB") (responseTxt Whoa),
    Case "asking a question" (T.pack "Does this cryogenic chamber make me look fat?") (responseTxt Sure),
    Case "asking a numeric question" (T.pack "You are, what, like 15?") (responseTxt Sure),
    Case "asking a question in yelling" (T.pack "TO THE WEST?") (responseTxt CalmDown),
    Case "talking forcefully" (T.pack "Let's go make out behind the gym!") (responseTxt Whatever),
    Case "silence" (T.pack "") (responseTxt Fine),
    Case "prolonged silence" (T.pack "   ") (responseTxt Fine),
    Case "only numbers" (T.pack "1, 2, 3") (responseTxt Whatever),
    Case "shouting numbers" (T.pack "1, 2, 3 GO!") (responseTxt Whoa),
    Case "question with no letters" (T.pack "4?") (responseTxt Sure)
  ]

main :: IO ()
main = defaultMain bobTests
