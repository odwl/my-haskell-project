{-# LANGUAGE BangPatterns #-}
module Fib.Algo where

import Data.Bits (testBit, finiteBitSize, countLeadingZeros)
import Data.WideWord.Word128 (Word128)
import Data.Word (Word32)

fib :: Word32 -> Word128
fib n = go n 0 1
  where
    go 0 !a _ = a
    go !k !a !b = go (k - 1) b (a + b)

fibFold :: Word32 -> Word128
fibFold n = fst $ foldl' step (0, 1) [1..n]
  where
    step (!a, !b) _ = (b, a + b)

data FibPair = FibPair
    { fCurr :: {-# UNPACK #-} !Word128
    , fNext :: {-# UNPACK #-} !Word128
    } deriving (Show, Eq)

fibLog :: Word32 -> Word128
fibLog n = fCurr $ go n
  where 
    go 0 = FibPair 0 1
    go !k = let !(FibPair fk fk1) = go (k `div` 2)
                !f2k = fk * ((fk1 + fk1) - fk)
                !f2k1 = fk * fk + fk1 * fk1
            in if even k then FibPair f2k f2k1 else FibPair f2k1 (f2k + f2k1)

fibLogCPS :: Word32 -> Word128
fibLogCPS n = fCurr $ go n id
  where
    go 0 !cont = cont (FibPair 0 1)
    go !k !cont = go (k `div` 2) $ \pair ->
        let !(FibPair fk fk1) = pair
            !f2k = fk * ((fk1 + fk1) - fk)
            !f2k1 = fk * fk + fk1 * fk1
        in if even k then cont (FibPair f2k f2k1) 
                     else cont (FibPair f2k1 (f2k + f2k1))

-- The best still kind of readable.
fibLogFold :: Word32 -> Word128
fibLogFold n = fCurr $ foldr step (FibPair 0 1) bitIndices
  where
    bitIndices = [0..finiteBitSize n - 1 - countLeadingZeros n]

    step i (FibPair fk fk1) =
        let !f2k = fk * ((fk1 + fk1) - fk)
            !f2k1 = fk * fk + fk1 * fk1
        in if testBit n i then FibPair f2k1 (f2k + f2k1) 
                          else FibPair f2k f2k1
