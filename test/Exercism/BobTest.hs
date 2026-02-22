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
    listOf1,
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
      testProperty "not a question gets specific responses" prop_not_a_question
    ]

prop_silence :: Property
prop_silence =
  forAll (listOf (elements " \t\n")) $ \s ->
    hey s === responseString Fine

prop_yelling :: Property
prop_yelling =
  forAll (listOf1 (elements ['A' .. 'Z'])) $ \s ->
    hey s `elem` map responseString [Whoa, CalmDown]

-- Verifies that any input ending with a question mark (followed by optional whitespace)
-- is treated as a question by Bob.
prop_question_finished_by :: Property
prop_question_finished_by = forAll genAQuestion $ \input -> elem (hey input) $ map responseString [CalmDown, Sure]

prop_not_a_question :: Property
prop_not_a_question = forAll genNotAQuestion $ \input -> elem (hey input) $ map responseString [Whoa, Whatever]

genAQuestion :: Gen String
genAQuestion = do
  s <- arbitrary :: Gen String
  noise <- genNoise
  pure (s ++ "?" ++ noise)

genNoise :: Gen String
genNoise = listOf (elements (filter isSpace ['\0' .. '\127']))

genNotAQuestion :: Gen String
genNotAQuestion = do
  s <- arbitrary :: Gen String
  nonSpaceChar <- elements (filter (\c -> not (isSpace c) && c /= '?') ['\0' .. '\127'])
  noise <- listOf (elements (filter isSpace ['\0' .. '\127']))
  pure (s ++ [nonSpaceChar] ++ noise)

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
