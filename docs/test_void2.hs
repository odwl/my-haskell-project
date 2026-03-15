module Test2 where
import Data.Void

eq1 :: Void -> Void -> Bool
eq1 = absurd

eq2 :: Void -> Void -> Bool
eq2 = absurd . const

eq3 :: Void -> Void -> Bool
eq3 = const absurd
