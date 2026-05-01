{-# LANGUAGE GADTs #-}
module Lambda.SandBox where

import Control.Arrow (Arrow (..), Kleisli (..), (>>>))
import Data.List (isPrefixOf, tails)
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Lambda (safeHead)
import qualified Safe
import qualified Control.Category as C

-- | splits an even length list such as [1,2,3,4,5,6] -> ([1,2,3], [4,5,6])
halve :: [a] -> Maybe ([a], [a])
halve list
  | even (length list) = Just (splitAt (length list `div` 2) list)
  | otherwise = Nothing

-- | get the third element of a list
third' :: [a] -> Maybe a
third' (_ : _ : x : _) = Just x
third' _ = Nothing

third :: [a] -> Maybe a
third = safeHead . drop 2

-- | behaves in the same way as tail except that
-- it maps the empty list to itself rather than producing an error
sTail :: [a] -> [a]
sTail [] = []
sTail (_ : xs) = xs

sTail' :: [a] -> [a]
sTail' = fromMaybe [] . Safe.tailMay

sTail'' :: [a] -> [a]
sTail'' = drop 1

scalar :: (Num a) => [a] -> [a] -> a
-- scalar xs = zip xs >>> map (uncurry (*)) >>> sum
scalar = zipWith (*) >>> (>>> sum)

-- scalar v1 v2 = sum $ zipWith (*) v1 v2

-- -- Counts the number of occurence of True in a list
-- countTrue :: [Bool] -> Int
-- countTrue = filter id >>> length

-- Counts the number of occurence of a word w in a string s
count :: (Eq a) => [a] -> [a] -> Int
count w = tails >>> filter (isPrefixOf w) >>> length

count2 :: String -> String -> Int
count2 w = words >>> filter (== w) >>> length

printOrmolu :: IO ()
printOrmolu = do
  content <- readFile "ormolu.yaml"
  putStr content

countFile :: String -> FilePath -> IO ()
-- countFile w = readFile >>> fmap (count2 w) >>> (>>= print)
-- countFile w = readFile >>> fmap (count2 w) >=> print
-- countFile w = readFile >=> (count2 w >>> return) >=> print
-- countFile w = runKleisli (countArrow w)
-- countFile w = readFile >=> arr words >=> arr (filter (==w)) >=> arr length >=> print
-- countFile w = readFile >=> arr (count2 w) >=> print
countFile w = runKleisli $ Kleisli readFile >>> arr (count2 w) >>> Kleisli print

countArrow :: String -> Kleisli IO FilePath ()
countArrow w = Kleisli readFile >>> arr words >>> arr (filter (== w)) >>> arr length >>> Kleisli print

-- do
-- content <- readFile filePath
-- return $ count2 w content

newtype SF a b = SF {runSF :: [a] -> [b]}

instance C.Category SF where
  id = SF id
  (.) (SF lf) (SF lg) = SF (lf . lg) -- coerce
  --
  -- (>>>) (SF lf) (SF lg) = SF lg . SF lf
  -- (<<<) (SF lf) (SF lg) = SF (lg . lf)

data DiscreteArrows a b where
  Refl :: DiscreteArrows a a
instance C.Category DiscreteArrows where
  id = Refl
  Refl . Refl = Refl
 
instance Arrow SF where
  -- arr :: (b -> c) -> SF b c
  arr f = SF (map f)
  
  -- first :: SF b c -> SF (b, d) (c, d)
  first (SF lf) = SF $ unzip >>> first lf >>> uncurry zip
  -- (&&&) (SF lf) (SF lg) = SF $ zip <$> lf <*> lg
  -- (&&&) (SF lf) (SF lg) = SF $ liftA2 zip lf lg
  
  -- second :: SF b c -> SF (d, b) (d, c)
  second sf = arr swap >>> first sf >>> arr swap
  
  -- (&&&) :: SF b c -> SF b d -> SF b (c, d)
  (&&&) sf1 sf2 = arr (id &&& id) >>> first sf1 >>> second sf2
  
  -- (***) :: SF b c -> SF b' c' -> SF (b, b') (c, c')
  (***) sf1 sf2 = first sf1 >>> second sf2 
  -- (***) (SF lf) (SF lg) = SF $ unzip >>> lf *** lg >>> uncurry zip -- more efficient.

            
