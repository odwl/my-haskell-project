{-# LANGUAGE RecordWildCards #-}

module Exercism.BobTest (bobTests, main) where

import Exercism.Bob (hey)
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
      testProperty "yelling always gets a yelling response" prop_yelling
    ]

prop_silence :: Property
prop_silence =
  forAll (listOf (elements " \t\n")) $ \s ->
    hey s === "Fine. Be that way!"

prop_yelling :: Property
prop_yelling =
  forAll (listOf1 (elements ['A' .. 'Z'])) $ \s ->
    hey s `elem` ["Whoa, chill out!", "Calm down, I know what I'm doing!"]

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
  [ Case "stating something" "Tom-ay-to, tom-aaaah-to." "Whatever.",
    Case "shouting" "WATCH OUT!" "Whoa, chill out!",
    Case "shouting gibberish" "FCECDFCAAB" "Whoa, chill out!",
    Case "asking a question" "Does this cryogenic chamber make me look fat?" "Sure.",
    Case "asking a numeric question" "You are, what, like 15?" "Sure.",
    Case "asking a question in yelling" "TO THE WEST?" "Calm down, I know what I'm doing!",
    Case "talking forcefully" "Let's go make out behind the gym!" "Whatever.",
    Case "silence" "" "Fine. Be that way!",
    Case "prolonged silence" "   " "Fine. Be that way!",
    Case "only numbers" "1, 2, 3" "Whatever.",
    Case "shouting numbers" "1, 2, 3 GO!" "Whoa, chill out!",
    Case "question with no letters" "4?" "Sure."
  ]

main :: IO ()
main = defaultMain bobTests
