module Lambda (safeHead, fact) where  -- <--- Only exports 'safeHead'

import Data.Function (fix)

-- factorial using fix.
step f n = if n == 0 then 1 else n * f (n - 1)
fact = fix step

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x