{-# LANGUAGE RecordWildCards #-}

module Exercism.PangramTest (pangramTests) where

import Control.Monad (forM)
import Data.Char (toLower, toUpper)
import Data.List (delete)
import Exercism.Pangram (isPangram)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    Property,
    counterexample,
    elements,
    forAll,
    listOf,
    shuffle,
    suchThat,
    testProperty,
    (===),
  )

pangramTests :: TestTree
pangramTests =
  testGroup
    "Pangram"
    [ quickCheckPangramTests,
      exampleTests
    ]

quickCheckPangramTests :: TestTree
quickCheckPangramTests =
  testGroup
    "QuickCheck"
    [ testProperty "pangram remains pangram when prepending any string" prop_prependPangram,
      testProperty "empty string is never a pangram" $ not (isPangram ""),
      testProperty "missing letter is not a pangram" prop_missingLetterIsNotPangram,
      testProperty "guaranteed pangram is recognized" prop_guaranteedPangram
    ]

prop_prependPangram :: String -> Property
prop_prependPangram s =
  let alphabet = "abcdefghijklmnopqrstuvwxyz"
   in isPangram (s ++ alphabet) === True

prop_missingLetterIsNotPangram :: Property
prop_missingLetterIsNotPangram =
  forAll genMissingLetterString checkNotPangram
  where
    checkNotPangram (bannedChar, testString) =
      counterexample ("Banned letter: " ++ show bannedChar ++ "\nString: " ++ show testString) $
        isPangram testString === False

prop_guaranteedPangram :: Property
prop_guaranteedPangram =
  forAll genPangramWithNoise $ \s ->
    counterexample ("Pangram with noise: " ++ show s) $
      isPangram s === True

-- ==========================================
-- Test Some Examples
-- ==========================================

exampleTests :: TestTree
exampleTests =
  testGroup
    "Examples"
    [ testCase explanation $ isPangram input @?= expected
      | Case {..} <- cases
    ]

data Case = Case {explanation :: String, input :: String, expected :: Bool}

cases :: [Case]
cases =
  [ Case "empty sentence" "" False,
    Case "perfect lower case" "abcdefghijklmnopqrstuvwxyz" True
  ]

-- ==========================================
-- Generators
-- ==========================================

-- This generator builds strings that are guaranteed to be missing one specific letter and no others.
-- It includes uppercase, lowercase, numbers, unicodes and punctuation!
genMissingLetterString :: Gen (Char, String)
genMissingLetterString = do
  (banned_lower, toBeUsed) <- genBannedLetterAndRemainingAlphabet
  let safeCharGen = suchThat arbitrary ((/= banned_lower) . toLower)
  noise <- listOf safeCharGen
  shuffledString <- shuffle (noise ++ toBeUsed)
  pure (banned_lower, shuffledString)

-- Generates a tuple where the first element is a random lowercase letter (the "banned" letter),
-- and the second element is a list containing all other letters of the alphabet in both
-- lowercase and uppercase. This ensures the resulting list contains every letter except the banned one.
genBannedLetterAndRemainingAlphabet :: Gen (Char, [Char])
genBannedLetterAndRemainingAlphabet = do
  let alphabet = ['a' .. 'z']
  bannedLetter <- elements alphabet
  let remainingLetters = delete bannedLetter alphabet
  remainingLettersRandomCase <- forM remainingLetters $ \c -> elements [c, toUpper c]
  pure (bannedLetter, remainingLettersRandomCase)

-- Generates a string that is guaranteed to be a pangram by including all letters
-- in random cases and mixing them with arbitrary noise characters.
genPangramWithNoise :: Gen String
genPangramWithNoise = do
  let alphabet = ['a' .. 'z']
  alphabetRandomCase <- forM alphabet $ \c -> elements [c, toUpper c]
  noise <- listOf arbitrary
  shuffle (alphabetRandomCase ++ noise)
