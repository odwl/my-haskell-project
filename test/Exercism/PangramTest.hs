{-# LANGUAGE RecordWildCards #-}
module Exercism.PangramTest (pangramTests) where

import Data.Char (toUpper, toLower)
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

randomCase :: Char -> Gen Char
randomCase c = elements [toLower c, toUpper c]

-- This generator builds strings that are guaranteed to be missing one specific letter and no others.
-- It includes uppercase, lowercase, numbers, unicodes and punctuation!
genMissingLetterString :: Gen (Char, String) 
genMissingLetterString = do

-- Other idea 


    shuffleAlphabet <- shuffle $ zip ['a'..'z'] ['A'..'Z']
    let (bannedChar : baseChars) = shuffleAlphabet

    let banned_lower = fst bannedChar
    let safeCharGen = suchThat arbitrary ((/= banned_lower) . toLower)
    noise <- listOf safeCharGen

    let baseChars2 = concatMap (\(lower, upper) -> [lower, upper]) baseChars

    let combinedString = baseChars2 ++ noise
    
    shuffledString <- shuffle combinedString 
    pure (banned_lower, shuffledString)



    -- shuffledAlphabet <- shuffle (['a'..'z'])
    -- -- The end string will contain all letters but missingChar ind its upper case + some noises.
    -- let (missingChar : baseChars) = shuffledAlphabet
    -- coinFlip <- elements [True, False]



    
    -- requiredChars <- mapM (\c -> do
    --     coinFlip <- elements [True, False]
    --     pure (if coinFlip then c else toUpper c)
    --   ) baseChars
      
    -- let safeCharGen = suchThat arbitrary ((/= missingChar) . toLower)
    -- noise <- listOf safeCharGen
    
    -- -- We make sure to use 'requiredChars' here instead of 'baseChars'
    -- let combinedString = baseChars ++ noise
    
    -- shuffledString <- shuffle combinedString 
    -- pure (missingChar, shuffledString)

prop_missingLetterIsNotPangram :: Property
prop_missingLetterIsNotPangram = 
    forAll genMissingLetterString checkNotPangram
  where
    checkNotPangram (bannedChar, testString) = 
        counterexample ("Banned letter: " ++ show bannedChar ++ "\nString: " ++ show testString) $
        isPangram testString === False

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
