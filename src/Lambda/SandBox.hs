module Lambda.SandBox where

{-@ type EvenList a = {v:[a] | (len v) mod 2 == 0} @-}

-- | splits an even length list such as [1,2,3,4,5,6] -> ([1,2,3], [4,5,6])

{-@ halve :: EvenList a -> ([a], [a]) @-}
halve :: [a] -> ([a], [a])
halve list = splitAt (length list `div` 2) list

{-@ halve' :: EvenList a -> ([a], [a]) @-}
halve' :: [a] -> ([a], [a])
halve' [] = ([], [])
halve' (x : y : xs) = let (l, r) = halve' xs in (x : l, y : r)
halve' [_] = error "List must have even length"
