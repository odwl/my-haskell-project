module Exercism.ReverseString (reverseString) where

reverseString :: [a] -> [a]
reverseString = foldl' (flip (:)) []
