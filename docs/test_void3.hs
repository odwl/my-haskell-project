module Test3 where
import Data.Void

instance Eq Void where
    (==) _ _ = True
