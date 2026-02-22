{-# LANGUAGE RecordWildCards #-}

module Exercism.BobTest (bobTests, main) where

import Data.Char (isSpace)
import Exercism.Bob (ResponseType (..), hey, responseString)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck
  ( Property,
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
      testProperty "questions get specific responses" prop_question_finished_by
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
prop_question_finished_by :: String -> Property
prop_question_finished_by s =
  forAll noiseGen $ \noise ->
    elem (inputQuestion noise) (map responseString [CalmDown, Sure])
  where
    noiseGen = listOf (elements (filter isSpace ['\0' .. '\127']))
    inputQuestion noise = hey (s ++ "?" ++ noise)

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
