module Lambda.MiniWhileRepro where

import Lambda.MiniWhile

main :: IO ()
main = do
  let input = "x := 10; y := (x + 2)"
  print $ parseString input
