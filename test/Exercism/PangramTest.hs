{-# LANGUAGE RecordWildCards #-}
module Exercism.PangramTest (pangramTests) where

import Data.Char (toUpper)
import Data.List (delete)
import Exercism.Pangram (isPangram)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

pangramTests :: TestTree
pangramTests = testGroup "Pangram"
    [ quickCheckPangramTests
    , exampleTests
    ]
    
quickCheckPangramTests :: TestTree
quickCheckPangramTests = testGroup "QuickCheck"
    [ testProperty "pangram remains pangram when prepending any string" prop_prependPangram
    , testProperty "empty string is never a pangram" $ not (isPangram "")
    , testProperty "missing letter is not a pangram" prop_missingLetterIsNotPangram
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


-- This generator builds strings that are guaranteed to be missing one specific letter.
-- It includes uppercase, lowercase, numbers, and punctuation!
genMissingLetterString :: Gen (Char, String)
genMissingLetterString = do
    -- 1. Pick a random lowercase letter to ban
    missingChar <- elements ['a'..'z']
    
    -- 2. Create a pool of ALL printable ASCII characters (spaces, punctuation, letters)
    let allPrintable = [' '..'~']
        withoutLower = delete missingChar allPrintable
        allowedChars = delete (toUpper missingChar) withoutLower
    randomStr <- listOf (elements allowedChars)
    
    -- 5. Return both the banned letter and the resulting string
    pure (missingChar, randomStr)

-- ==========================================
-- Test Some Examples
-- ==========================================

exampleTests :: TestTree
exampleTests = testGroup "Examples"
    [ testCase explanation $ isPangram input @?= expected
    | Case{..} <- cases
    ]

data Case = Case { explanation :: String, input :: String, expected :: Bool }
cases :: [Case]
cases = [ Case "empty sentence" "" False
        , Case "perfect lower case" "abcdefghijklmnopqrstuvwxyz" True
        ]
