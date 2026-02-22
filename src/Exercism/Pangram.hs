module Exercism.Pangram (isPangram) where

import Data.Char

-- A pangram is a sentence using every letter of the alphabet at least once. It is case insensitive, so it doesn't matter if a letter is lower-case (e.g. k) or upper-case (e.g. K).
-- For this exercise, a sentence is a pangram if it contains each of the 26 letters in the English alphabet.

isPangram :: String -> Bool
isPangram text = all (flip elem lowerText) ['a' .. 'z']
  where
    lowerText = map toLower text

-- import Data.Char (isAlpha, isAscii, toLower)
-- import Data.Set (fromList, size)
-- isPangram = (== length ['a' .. 'z']) . size . fromList . map toLower . filter isAlpha . filter isAscii