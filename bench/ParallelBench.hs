{-# LANGUAGE OverloadedStrings #-}
module Main where

import Criterion.Main
import qualified Data.Text as Text
import Exercism.Parallel (frequency)

main :: IO ()
main = do
  let baseText = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. "
      texts = [ Text.replicate n baseText | n <- [1..1000] ]
  defaultMain
    [ bgroup "frequency"
        [ bench "1 worker"   $ nf (frequency 1) texts
        , bench "4 workers"  $ nf (frequency 4) texts
        , bench "8 workers"  $ nf (frequency 8) texts
        , bench "16 workers" $ nf (frequency 16) texts
        , bench "32 workers" $ nf (frequency 32) texts
        ]
    ]
