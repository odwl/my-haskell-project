module Exercism.AnagramTest (anagramTests, main) where

import Data.Char
import Data.List
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

prop_shuffle :: String -> Property
prop_shuffle s =
  length (nub lowerS) > 1 ==>
    forAll (suchThat (shuffle s) ((/= lowerS) . map toLower)) (anagramOf s)
  where
    lowerS = map toLower s

prop_mostly_anagram :: Property
prop_mostly_anagram =
  forAll genMostlyAnagram $ not . uncurry anagramOf

genMostlyAnagram :: Gen (String, String)
genMostlyAnagram = do
  s1 <- suchThat arbitrary (not . null)
  let lowerS1 = map toLower s1
  char <- suchThat arbitrary $ \c -> notElem (toLower c) lowerS1
  fmap ((,) s1) $ shuffle (char : tail s1)

main :: IO ()
main = defaultMain anagramTests
