module Lambda.SandBox where

import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Traversable (mapAccumR)
import Lambda (safeHead)
import qualified Safe

-- | splits an even length list such as [1,2,3,4,5,6] -> ([1,2,3], [4,5,6])
halve :: [a] -> Maybe ([a], [a])
halve list
  | even (length list) = Just (splitAt (length list `div` 2) list)
  | otherwise = Nothing

-- | get the third element of a list
third' :: [a] -> Maybe a
third' (_ : _ : x : _) = Just x
third' _ = Nothing

third :: [a] -> Maybe a
third = safeHead . drop 2
 
-- | behaves in the same way as tail except that
-- it maps the empty list to itself rather than producing an error
sTail :: [a] -> [a]
sTail [] = []
sTail (_ : xs) = xs

sTail' :: [a] -> [a]
sTail' = fromMaybe [] . Safe.tailMay

sTail'' :: [a] -> [a]
sTail'' = drop 1


