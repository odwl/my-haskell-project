{-# LANGUAGE RecordWildCards #-}

module Exercism.BobTest (bobTests, main) where

import Data.Char (isSpace)
import Exercism.Bob (ResponseType (..), hey, responseString)
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
  forAll genNoise $ \noise ->
    hey noise === responseString Fine

prop_not_silence :: Property
prop_not_silence =
  forAll genNotSilence $ \input ->
    hey input /= responseString Fine

prop_yelling :: Property
prop_yelling =
  forAll genYelling $ \input ->
    hey input `elem` map responseString [Whoa, CalmDown]

-- Verifies that any input ending with a question mark (followed by optional whitespace)
-- is treated as a question by Bob.
prop_question_finished_by :: Property
prop_question_finished_by = forAll genAQuestion $ \input -> elem (hey input) $ map responseString [CalmDown, Sure]

prop_not_a_question :: Property
prop_not_a_question = forAll genNotAQuestion $ \input -> elem (hey input) $ map responseString [Whoa, Whatever]

-- Example: "How are you?  "
genAQuestion :: Gen String
genAQuestion = do
  s <- arbitrary :: Gen String
  noise <- genNoise
  pure (s ++ "?" ++ noise)

-- Example: "  \t"
genNoise :: Gen String
genNoise = listOf (elements (filter isSpace ['\0' .. '\127']))

-- Example: "Hello!"
genNotAQuestion :: Gen String
genNotAQuestion = do
  s <- arbitrary :: Gen String
  nonSpaceChar <- genNonSpaceChar
  noise <- genNoise
  pure (s ++ [nonSpaceChar] ++ noise)

-- Example: 'A'
genNonSpaceChar :: Gen Char
genNonSpaceChar = elements $ filter (\c -> not (isSpace c) && c /= '?') ['\0' .. '\127']

-- Example: " a "
genNotSilence :: Gen String
genNotSilence = do
  n1 <- genNoise
  c <- genNonSpaceChar
  n2 <- genNoise
  pure (n1 ++ [c] ++ n2)

-- Example: "1, 2, 3 GO!  "
genYelling :: Gen String
genYelling = do
  -- Base noise (spaces) and punctuation/numbers
  baseNoise <- listOf (elements (filter (\c -> isSpace c || (c >= '0' && c <= '9') || c `elem` ("!@#$%^&*,." :: String)) ['\0' .. '\127']))
  -- We MUST have at least one uppercase letter
  upperCaseLetter <- elements ['A' .. 'Z']
  -- Optional additional uppercase letters
  moreUpperCase <- listOf (elements ['A' .. 'Z'])

  -- Shuffle everything or just append nicely? Let's spread it nicely.
  -- To keep it simple but rigorous, we'll embed the uppercase letters inside the noise
  pure (baseNoise ++ [upperCaseLetter] ++ moreUpperCase ++ baseNoise)

-- ==========================================
-- Example Tests
-- ==========================================

exampleTests :: TestTree
exampleTests =
  testGroup
    "Examples"
    [ testCase explanation $ hey input @?= expected
      | Case {..} <- cases
    ]

data Case = Case {explanation :: String, input :: String, expected :: String}

cases :: [Case]
cases =
  [ Case "stating something" "Tom-ay-to, tom-aaaah-to." (responseString Whatever),
    Case "shouting" "WATCH OUT!" (responseString Whoa),
    Case "shouting gibberish" "FCECDFCAAB" (responseString Whoa),
    Case "asking a question" "Does this cryogenic chamber make me look fat?" (responseString Sure),
    Case "asking a numeric question" "You are, what, like 15?" (responseString Sure),
    Case "asking a question in yelling" "TO THE WEST?" (responseString CalmDown),
    Case "talking forcefully" "Let's go make out behind the gym!" (responseString Whatever),
    Case "silence" "" (responseString Fine),
    Case "prolonged silence" "   " (responseString Fine),
    Case "only numbers" "1, 2, 3" (responseString Whatever),
    Case "shouting numbers" "1, 2, 3 GO!" (responseString Whoa),
    Case "question with no letters" "4?" (responseString Sure)
  ]

main :: IO ()
main = defaultMain bobTests
