{-# LANGUAGE RecordWildCards #-}

module Exercism.ReverseStringTest (reverseStringTests, main) where

import Exercism.ReverseString (reverseString)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck
  ( Property,
    testProperty,
    (===),
    (==>),
  )

-- ==========================================
-- Master Test Tree
-- ==========================================

reverseStringTests :: TestTree
reverseStringTests =
  testGroup
    "reverseStringTests"
    [ quickCheckReverseTests,
      exampleTests
    ]

-- ==========================================
-- Test QuickChecks
-- ==========================================

quickCheckReverseTests :: TestTree
quickCheckReverseTests =
  testGroup
    "QuickCheck"
    [ testProperty "reverse" prop_reverseTwice,
      testProperty "length" prop_lengthPreserve,
      testProperty "concat" prop_reverseConcat,
      testProperty "reverseHead" prop_reverseHead
    ]

prop_reverseTwice :: String -> Property
prop_reverseTwice s = reverseString (reverseString s) === s

prop_lengthPreserve :: String -> Property
prop_lengthPreserve s = length (reverseString s) === length s

prop_reverseConcat :: String -> String -> Property
prop_reverseConcat a b = reverseString (a ++ b) === reverseString b ++ reverseString a

prop_reverseHead :: String -> Property
prop_reverseHead s =
  not (null s) ==> head (reverseString s) === last s

-- ==========================================
-- Test Some Examples
-- ==========================================

exampleTests :: TestTree
exampleTests =
  testGroup
    "Examples"
    [ testCase explanation $ reverseString input @?= expected
      | Case {..} <- cases
    ]

data Case = Case {explanation :: String, input :: String, expected :: String}

cases :: [Case]
cases =
  [ Case "an empty string" "" "",
    Case "a word" "robot" "tobor",
    Case "a capitalized word" "Ramen" "nemaR",
    Case "a sentence with punctuation" "I'm hungry!" "!yrgnuh m'I",
    Case "a palindrome" "racecar" "racecar",
    Case "an even-sized word" "drawer" "reward"
  ]

main :: IO ()
main = defaultMain reverseStringTests