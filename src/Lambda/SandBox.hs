module Lambda.SandBox where

{-@ type EvenList a = {v:[a] | (len v) mod 2 == 0} @-}

-- | splits an even length list such as [1,2,3,4,5,6] -> ([1,2,3], [4,5,6])

{-@ halve :: EvenList a -> ([a], [a]) @-}
halve :: [a] -> ([a], [a])
halve list = splitAt (length list `div` 2) list

-- | get the third element of a list
third :: [a] -> a
third l = l !! 2

-- | get the third element of a list
third' :: [a] -> a
third' (_ : _ : x : _) = x

third'' :: [a] -> a
third'' = head . tail . tail

badHalve :: ([Int], [Int])
badHalve = halve [1, 2, 3]
