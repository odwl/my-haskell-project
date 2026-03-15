module TestExercise where
import Data.Void

eq1 :: Void -> Void -> Bool
eq1 = const absurd

eq2 :: Void -> Void -> Bool
eq2 v = absurd

