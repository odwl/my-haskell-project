module Exercism.ReverseString (reverseString) where

import Data.List ( foldl' )

reverseString :: [a] -> [a]
reverseString = foldl' (\acc x -> x:acc) []

-- reverseStringSlow :: [a] -> [a]
-- reverseStringSlow xs = go [] xs where 
--     go acc [] = acc
--     go acc (x:rest) = go (x:acc) rest


-- reverseStringSlow [] = []
-- reverseStringSlow (x:rest) = reverseString rest ++ [x]