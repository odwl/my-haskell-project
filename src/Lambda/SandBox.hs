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

luhn :: [Int] -> Bool
luhn = (== 0) . (`mod` 10) . luhnSum
    where luhnSum [] = 0
          luhnSum (x : xs) = (if odd (length xs) then if x >= 5 then 2 * x - 9 else 2 * x else x) + luhnSum xs

luhn' :: [Int] -> Bool
luhn' list = (== 0) . (`mod` 10) $ luhnAcc 0 (even (length list)) list

luhnAcc :: Int -> Bool -> [Int] -> Int
luhnAcc a False (x : xs) = luhnAcc (a + x) True xs
luhnAcc a True (x : xs) | x < 5 = luhnAcc (a + 2 * x) False xs
luhnAcc a True (x : xs) | x >= 5 = luhnAcc (a + 2 * x - 9) False xs
luhnAcc a _ [] = a
luhnAcc _ _ _ = 0

luhn'' :: [Int] -> Bool
luhn'' list = (== 0) . (`mod` 10) . fst $ result 
      where 
        initState = (0, even (length list))
        result = foldl' step initState list
        step (a, False) x = (a + x, True)
        step (a, True) x = (a + 2 * x - if x >= 5 then 9 else 0, False)

luhn''' :: [Int] -> Bool
luhn''' = (== 0) . (`mod` 10) . sum . toSum
    where fn (False, val) = val
          fn (True, val) = 2 * val - if val >= 5 then 9 else 0
          toSum list = map fn $ zip (cycle [False, True]) (reverse list)

luhn'''' :: [Int] -> Bool
luhn'''' list = (== 0) . (`mod` 10) . sum . toSum $ list
  where fn val = 2 * val - if val >= 5 then 9 else 0
        isFirstDouble = even (length list)
        c = drop (fromEnum isFirstDouble) $ cycle [id, fn]
        toSum = zipWith ($) c

luhn''''' :: [Int] -> Bool
luhn''''' = (== 0) . (`mod` 10) . fst . foldr step (0, False)
  where step x (total, hasExtra) = (total + toAdd hasExtra x, not hasExtra)
        toAdd False x = x
        toAdd True x = 2 * x - if x >= 5 then 9 else 0

luhnMapAccumR :: [Int] -> Bool
luhnMapAccumR xs = (== 0) . (`mod` 10) . sum . snd $ mapAccumR step False xs
  where step hasExtra x = (not hasExtra, toAdd hasExtra x)
        toAdd False x = x
        toAdd True x = 2 * x - if x >= 5 then 9 else 0

luhnMapAccumRSeparated :: [Int] -> Bool
luhnMapAccumRSeparated = (== 0) . (`mod` 10) . sum . snd . foldr step (False, [])
        where step x (double, dx) = (not double, (if double then handleGreater (2 * x) else x) : dx)
              handleGreater x = if x >= 10 then x - 9 else x
