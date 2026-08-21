module Main where

import Lambda.RequiredTypeArguments (printStorableExamples)

main :: IO ()
main = do
  putStrLn "================================================================="
  putStrLn "GHC 9.10 RequiredTypeArguments & Storable Crash Course Demo"
  putStrLn "=================================================================\n"
  printStorableExamples
