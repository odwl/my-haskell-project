module Test4 where
import Data.Void

data MyVoid

instance Eq MyVoid where
    v1 == _ = absurd (unsafeCoerce v1)
      where unsafeCoerce = undefined
    
