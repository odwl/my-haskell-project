{-# LANGUAGE BangPatterns #-}
module Main where

import System.CPUTime
import Text.Printf
import Control.Exception
import Fib.Algo (fibFold, fibLog, fibLogFold)
import Data.Word (Word32)
import Data.WideWord.Word128 (Word128)

import System.Environment (getArgs)

-- NOINLINE double barriers for forcing honest, uncached loop evaluation across runs
{-# NOINLINE identity #-}
identity :: Word32 -> Int -> Word32
identity x _ = x

{-# NOINLINE runOne #-}
runOne :: (Word32 -> Word128) -> Word32 -> Int -> IO Word128
runOne f x i = evaluate (f (identity x i))

timeFuncMemoized :: String -> (Word32 -> Word128) -> Word32 -> IO ()
timeFuncMemoized label func n = do
    let iters = 10000 :: Int
    val <- evaluate (func n)
    printf "%s: %s\n" label (show val)

    start <- getCPUTime
    let loop 0 !acc = evaluate acc
        loop i _ = do
            !r <- evaluate (func n)
            loop (i - 1) r
    _ <- loop iters 0
    end <- getCPUTime

    let totalNs = fromIntegral (end - start) / (1000.0 :: Double)
    let avgNs = totalNs / fromIntegral iters
    printf "Average over %d runs: %.2f ns per calculation\n" iters avgNs

timeFuncUncached :: String -> (Word32 -> Word128) -> Word32 -> IO ()
timeFuncUncached label func n = do
    let iters = 10000 :: Int
    val <- evaluate (func n)
    printf "%s: %s\n" label (show val)

    start <- getCPUTime
    let loop 0 !acc = evaluate acc
        loop i _ = do
            !r <- runOne func n i
            loop (i - 1) r
    _ <- loop iters 0
    end <- getCPUTime

    let totalNs = fromIntegral (end - start) / (1000.0 :: Double)
    let avgNs = totalNs / fromIntegral iters
    printf "Average over %d runs: %.2f ns per calculation\n" iters avgNs

main :: IO ()
main = do
    args <- getArgs
    let n = 1000000 :: Word32
    case args of
        ["--memoized"] -> timeFuncMemoized "fibFold(1,000,000) [O(N)]" fibFold n
        ["--uncached"] -> timeFuncUncached "fibFold(1,000,000) [O(N)]" fibFold n
        ["--fibLog"]   -> do
            timeFuncMemoized "fibLog(1,000,000) [O(log N)] (Memoized)" fibLog n
            timeFuncUncached "fibLog(1,000,000) [O(log N)] (Uncached Recursive)" fibLog n
            timeFuncUncached "fibLogFold(1,000,000) [O(log N)] (Uncached Fold)" fibLogFold n
        _ -> do
            putStrLn "--- Haskell Fibonacci Timing (-O2 across 10,000 runs) ---"
            timeFuncMemoized "fibFold(1,000,000) [O(N)] (Memoized)" fibFold n
            timeFuncUncached "fibFold(1,000,000) [O(N)] (Uncached)" fibFold n
            timeFuncUncached "fibLog(1,000,000) [O(log N)] (Uncached Recursive)" fibLog n
            timeFuncUncached "fibLogFold(1,000,000) [O(log N)] (Uncached Fold)" fibLogFold n




