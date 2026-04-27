module Exercism.Luhn (isValid, parseInput, luhn) where

import Control.Arrow ((>>>))
import Control.Monad ((>=>))
import Data.Char (isSpace)
import Text.Read (readMaybe)

isValid :: String -> Bool
isValid = parseInput >>> any luhn

-- Parsing and Validation: fails fast and runs in O(N)
parseInput :: String -> Maybe [Int]
parseInput = filter (not . isSpace) >>> validLength >>> (>>= traverse (readMaybe . pure))
  where
    validLength ns@(_ : _ : _) = Just ns
    validLength _ = Nothing

-- single-pass foldr, no list reversals
luhn :: [Int] -> Bool
luhn = foldr step (0, False) >>> fst >>> (`rem` 10) >>> (== 0)
  where
    step x (total, hasExtra) = (total + toAdd hasExtra x, not hasExtra)
    toAdd False x = x
    toAdd True x = 2 * x - if x >= 5 then 9 else 0

