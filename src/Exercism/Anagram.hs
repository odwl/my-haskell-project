module Exercism.Anagram (anagramsFor, anagramOf) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor = filter . anagramOf

anagramOf :: String -> String -> Bool
anagramOf s1 s2 = lower1 /= lower2 && sort lower1 == sort lower2
  where
    lower1 = map toLower s1
    lower2 = map toLower s2
