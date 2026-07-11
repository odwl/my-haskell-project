{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Exercism.LuhnTest (luhnTests) where

import Data.Char (isSpace, isDigit)
import Exercism.Luhn (isValid, parseInput, parseInputIgnore, luhn)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, (==>))

luhnTests :: TestTree
luhnTests = testGroup "Luhn"
  [ testGroup "Exercism Cases" $ map test cases
  , testGroup "parseInputIgnore Cases"
      [ testCase "ignores non-digits" $
          parseInputIgnore "05a9" @?= Just [0, 5, 9]
      , testCase "ignores all non-digits if long enough" $
          parseInputIgnore "0 5 a 9" @?= Just [0, 5, 9]
      , testCase "fails if too short after ignoring" $
          parseInputIgnore "0a" @?= Nothing
      , testCase "fails if empty after ignoring" $
          parseInputIgnore "a" @?= Nothing
      ]
  , properties
  ]
  where
    test Case{..} = testCase description $ isValid input @?= expected

properties :: TestTree
properties = testGroup "QuickCheck Properties"
  [ testProperty "parseInput ignores whitespace" $
      \(xs :: String) -> parseInput xs == parseInput (filter (not . isSpace) xs)
  , testProperty "parseInput returns Nothing if it contains non-digits" $
      \(xs :: String) -> any (\c -> not (isDigit c || isSpace c)) xs ==> parseInput xs == Nothing
  , testProperty "luhn returns True for all zeros (len >= 2)" $
      \(n :: Int) -> (n >= 2 && n < 100) ==> luhn (replicate n 0) == True
  ]

data Case = Case { description :: String
                 , input       :: String
                 , expected    :: Bool
                 }

cases :: [Case]
cases = [ Case { description = "single digit strings can not be valid"
               , input       = "1"
               , expected    = False
               }
        , Case { description = "a single zero is invalid"
               , input       = "0"
               , expected    = False
               }
        , Case { description = "a simple valid SIN that remains valid if reversed"
               , input       = "059"
               , expected    = True
               }
        , Case { description = "a simple valid SIN that becomes invalid if reversed"
               , input       = "59"
               , expected    = True
               }
        , Case { description = "a valid Canadian SIN"
               , input       = "055 444 285"
               , expected    = True
               }
        , Case { description = "invalid Canadian SIN"
               , input       = "055 444 286"
               , expected    = False
               }
        , Case { description = "invalid credit card"
               , input       = "8273 1232 7352 0569"
               , expected    = False
               }
        , Case { description = "invalid long number with an even remainder"
               , input       = "1 2345 6789 1234 5678 9012"
               , expected    = False
               }
        , Case { description = "valid number with an even number of digits"
               , input       = "095 245 88"
               , expected    = True
               }
        , Case { description = "valid number with an odd number of spaces"
               , input       = "234 567 891 234"
               , expected    = True
               }
        , Case { description = "single zero with space is invalid"
               , input       = " 0"
               , expected    = False
               }
        , Case { description = "more than a single zero is valid"
               , input       = "0000 0"
               , expected    = True
               }
        , Case { description = "input digit 9 is correctly converted to output digit 9"
               , input       = "091"
               , expected    = True
               }
        ]


