module Exercism.Pangram (isPangram) where

import Data.Set (fromList, size) 
import Data.Char (toLower, isAlpha, isAscii)

-- A pangram is a sentence using every letter of the alphabet at least once. It is case insensitive, so it doesn't matter if a letter is lower-case (e.g. k) or upper-case (e.g. K).
-- For this exercise, a sentence is a pangram if it contains each of the 26 letters in the English alphabet.

isPangram :: String -> Bool
isPangram = (== 26) . size . fromList . map toLower . filter isAlpha . filter isAscii