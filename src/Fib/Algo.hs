{-# LANGUAGE BangPatterns #-}
module Fib.Algo where

import Data.Bits (testBit, finiteBitSize, countLeadingZeros)
import Data.List (foldl')
import Data.Word (Word32)
import Data.WideWord.Word128 (Word128)

fib :: Word32 -> Word128
fib n = go n 0 1
  where
    go 0 !a _ = a
    go !k !a !b = go (k - 1) b (a + b)

fibFold :: Word32 -> Word128
fibFold n = fst $ foldl' step (0, 1) [1..n]
  where
    step (!a, !b) _ = (b, a + b)

fibLog :: Word32 -> Word128
fibLog n = fst $ go n where 
    go 0 = (0, 1)
    go k = let !(!a, !b) = go (k `div` 2)
               !c = a * (2 * b - a)
               !d = a * a + b * b
           in if even k then (c, d) else (d, c + d)

data FibPair = FibPair
    { fCurr :: {-# UNPACK #-} !Word128
    , fNext :: {-# UNPACK #-} !Word128
    } deriving (Show, Eq)

-- The best still kind of readable.
fibLogFold :: Word32 -> Word128
fibLogFold n = fCurr $ foldr step (FibPair 0 1) bitIndices
  where
    bitIndices = [0..finiteBitSize n - 1 - countLeadingZeros n]

    step i (FibPair a b) =
        let !f2k = a * ((b + b) - a)
            !f2k1 = a * a + b * b
        in if testBit n i then FibPair f2k1 (f2k + f2k1) 
                          else FibPair f2k f2k1
