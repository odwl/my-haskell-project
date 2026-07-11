module Exercism.Parallel (frequency) where

import Control.Parallel.Strategies (parList, rseq, using)
import Data.Char (isAlpha, toLower)
import Data.Map (Map)
import Data.Text (Text, foldl')
import qualified Data.List as List
import qualified Data.Map as Map

-- | Calculates the total frequency of each letter in a list of texts using parallel computation.
-- Work is partitioned into 'nWorkers' batches, each evaluated in parallel across GHC sparks.
frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts = treeUnion (map mergeChunk textBatches `using` parList rseq)
  where
    chunkSize = max 1 (length texts `div` nWorkers)
    textBatches = chunksOf chunkSize texts
    mergeChunk = List.foldl' (Map.unionWith (+)) Map.empty . map task

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = prefix : chunksOf n suffix
  where (prefix, suffix) = splitAt n xs

-- | Merges a list of frequency maps in parallel using a binary reduction tree.
-- This divide-and-conquer approach eliminates head-of-line blocking and distributes map unions across cores.
treeUnion :: [Map Char Int] -> Map Char Int
treeUnion [] = Map.empty
treeUnion [m] = m
treeUnion ms = treeUnion (triples ms `using` parList rseq)
  where
    triples (x:y:z:rest) = Map.unionWith (+) (Map.unionWith (+) x y) z : triples rest
    triples [x, y] = [Map.unionWith (+) x y]
    triples xs = xs

-- | Scans a single 'Text' chunk to count character frequencies.
-- Executes an in-place strict left fold, filtering for alphabetic characters and converting to lowercase.
task :: Text -> Map Char Int 
task txt = Data.Text.foldl' step Map.empty txt 
  where step :: Map Char Int -> Char -> Map Char Int
        step m k 
          | isAlpha k = Map.insertWith (+) (toLower k) 1 m
          | otherwise = m
