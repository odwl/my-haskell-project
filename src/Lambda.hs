module Lambda (safeHead, fact, addMaybes) where 

import Data.Function (fix)
import Control.Applicative (liftA2)

-- factorial using fix.
step f n = if n == 0 then 1 else n * f (n - 1)
fact = fix step

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

addMaybes :: Maybe Int -> Maybe Int -> Maybe Int
-- addMaybes (Just x) (Just y) = Just (x + y)
-- addMaybes _ _ = Nothing
addMaybes = liftA2 (+)
