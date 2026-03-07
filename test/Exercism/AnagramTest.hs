{-# LANGUAGE TupleSections #-}

module Exercism.AnagramTest (anagramTests, main) where

import Data.Char
import Exercism.Anagram (anagramOf, anagramsFor)
import Test.QuickCheck
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck

anagramTests :: TestTree
anagramTests = testGroup "Anagram" [anagramForTests, anagramOfTests, quickCheckAnagramTests]

-- ==========================================
-- Example Tests
-- ==========================================

anagramForTests :: TestTree
anagramForTests =
  testGroup
    "Anagram"
    [ testCase "no matches" $ anagramsFor "diaper" ["hello", "world", "zombies", "pants"] @?= [],
      testCase "one matche" $ anagramsFor "diaper" ["hello", "perdia", "zombies", "pants"] @?= ["perdia"],
      testCase "two matches" $ anagramsFor "diaper" ["hello", "perdia", "zombies", "adiper"] @?= ["perdia", "adiper"]
    ]

anagramOfTests :: TestTree
anagramOfTests =
  testGroup
    "Anagram"
    [ testCase "no matches" $ anagramOf "diaper" "hello" @?= False,
      testCase "one match" $ anagramOf "diaper" "perdia" @?= True,
      testCase "case-insensitive self-matching" $ anagramOf "mM" "Mm" @?= False
    ]

-- ==========================================
-- QuickCheck Properties
-- ==========================================

quickCheckAnagramTests :: TestTree
quickCheckAnagramTests =
  testGroup
    "QuickCheck"
    [ testProperty "id not anagram" prop_id,
      testProperty "shuffle invariant" prop_shuffle,
      testProperty "mostly anagram" prop_mostly_anagram
    ]

prop_id :: String -> Property
prop_id s = property $ not $ anagramOf s s

prop_shuffle :: Char -> String -> Property
prop_shuffle c s =
  (not (null s) && toLower c `notElem` map toLower s) ==>
    forAll (suchThat (shuffle word) ((/= lowerWord) . map toLower)) (anagramOf word)
  where
    word = c : s
    lowerWord = map toLower word

prop_mostly_anagram :: Property
prop_mostly_anagram =
  forAll genMostlyAnagram $ not . uncurry anagramOf

genMostlyAnagram :: Gen (String, String)
genMostlyAnagram = do
  s1 <- suchThat arbitrary (not . null)
  let lowerS1 = map toLower s1
  char <- suchThat arbitrary (\c -> toLower c `notElem` lowerS1)
  let s1Tail = case s1 of
        (_ : ys) -> ys
        [] -> error "genMostlyAnagram: s1 should not be empty" -- This case is prevented by suchThat (not . null)
  fmap (s1,) $ shuffle (char : s1Tail)

main :: IO ()
main = defaultMain anagramTests
