{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module CountDown (Op(..), allOps, apply, valid, Positive, mkPositive, unPositive, (.+), (.*), Expr(Val), mkApp, one, values, eval, choices, split, combine, exprs, solve, solutions, main) where

import Data.List (subsequences, permutations)
import Data.Bifunctor (first)
import Data.Maybe (maybeToList)
import Control.Monad (guard)
import Control.Category ((>>>))
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.MonoTraversable (olength, onull, headEx, Element, otoList)
import Data.Sequences (IsSequence, splitAt, Index, fromList)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U


--------------------------------------------------------------------------------
-- Positive Numbers & Domain Logic
--------------------------------------------------------------------------------

newtype Positive = Positive { unPositive :: Int }
  deriving (Eq, Ord)

instance Show Positive where
  show (Positive x) = show x

mkPositive :: Int -> Maybe Positive
mkPositive n
  | n > 0     = Just (Positive n)
  | otherwise = Nothing

isMultipleOf :: Positive -> Positive -> Bool
isMultipleOf (Positive x) (Positive y) = x `mod` y == 0

one :: Positive 
one = Positive 1

(.+) :: Positive -> Positive -> Positive
(Positive x) .+ (Positive y) = Positive (x + y)
infixl 6 .+

(.*) :: Positive -> Positive -> Positive
(Positive x) .* (Positive y) = Positive (x * y)
infixl 7 .*

--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

data Op where
  Add :: Op
  Sub :: Op
  Mul :: Op
  Div :: Op
  deriving (Eq, Enum, Bounded)

allOps :: [Op]
allOps = [Add, Sub, Mul, Div]

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

-- Hutton's Optimization: 
-- Enforcing `x <= y` for commutative operations drastically prunes the search tree.
valid :: Op -> Positive -> Positive -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x <= y && x /= one
valid Div x y = isMultipleOf x y && y /= one

apply :: Op -> Positive -> Positive -> Positive
apply Add (Positive x) (Positive y) = Positive (x + y)
apply Sub (Positive x) (Positive y) = Positive (x - y)
apply Mul (Positive x) (Positive y) = Positive (x * y)
apply Div (Positive x) (Positive y) = Positive (x `div` y)

--------------------------------------------------------------------------------
-- Expressions & Evaluation
--------------------------------------------------------------------------------
data Expr where 
  Val :: Positive -> Expr
  App :: Op -> Expr -> Expr -> Positive -> Expr
  deriving (Eq)

mkApp :: Op -> Expr -> Expr -> Maybe Expr
mkApp op l r = do
  guard (valid op vl vr)
  pure (App op l r (apply op vl vr))
  where
    vl = eval l
    vr = eval r

instance Show Expr where 
  show (Val x) = show x
  show (App op l r _) = brak l ++ show op ++ brak r
    where brak (Val y) = show y
          brak expr = "(" ++ show expr ++ ")"

values :: Expr -> [Positive]
values (Val x) = [x]
values (App _ l r _) = values l ++ values r

eval :: Expr -> Positive
eval (Val n) = n
eval (App _ _ _ val) = val

--------------------------------------------------------------------------------
-- Brute Force Search
--------------------------------------------------------------------------------

-- `choices` returns all possible permutations of all possible sub-sequences of a given sequence.
-- This represents picking any subset of numbers in any possible order.
choices :: IsSequence seq => seq -> [seq]
choices = otoList
      >>> subsequences
      >>> concatMap permutations
      >>> map fromList

-- `split` generates all possible ways to divide a sequence into two non-empty halves 
-- without changing the order of the elements. Works generically over any IsSequence container!
split :: (IsSequence seq, Index seq ~ Int) => seq -> [(seq, seq)]
split xs = do
  i <- [1 .. olength xs - 1]
  pure (Data.Sequences.splitAt i xs)

-- `combine` takes a left and right expression and attempts to join them 
-- using every possible operator.
combine :: Expr -> Expr -> [Expr]
combine l r = do
  op <- allOps
  maybeToList (mkApp op l r)

-- `exprs` generates every perfectly valid mathematical tree that can be 
-- formed from a given sequence of numbers.
exprs :: (IsSequence seq, Index seq ~ Int, Element seq ~ Positive) => seq -> [Expr]
exprs ns
  | onull ns        = []
  | olength ns == 1 = [Val (headEx ns)]
  | otherwise        = do 
      (ls, rs) <- split ns 
      l <- exprs ls
      r <- exprs rs
      combine l r

solve :: (IsSequence seq, Index seq ~ Int, Element seq ~ Positive) => seq -> Positive -> [Expr]
solve ns target = do 
  choice <- choices ns 
  e <- exprs choice 
  guard (eval e == target)
  pure e

-- `solutions` is a friendly wrapper taking standard Int inputs and returning all valid Exprs.
solutions :: [Int] -> Int -> [Expr]
solutions ns target =
  case (mapM mkPositive ns, mkPositive target) of
    (Just ps, Just t) -> solve ps t
    _                 -> []

--------------------------------------------------------------------------------
-- Timing & Main Entry Point
--------------------------------------------------------------------------------

timeAction :: String -> IO a -> IO a
timeAction label action = do
  start <- getCurrentTime
  result <- action
  end <- getCurrentTime
  putStrLn $ label ++ ": " ++ show (diffUTCTime end start)
  pure result

main :: IO ()
main = do
  let nums = [1, 3, 7, 10, 25, 50]
  let Just ps = mapM mkPositive nums
  let Just t = mkPositive 765

  putStrLn "=== Target 765 (Standard List []) ==="
  timeAction "First solution (List)" $ print (take 1 $ solve ps t)
  timeAction "All solutions (List)" $ do
    let sols = solve ps t
    putStrLn $ "Total count: " ++ show (length sols)

  putStrLn "\n=== Target 765 (Data.Vector Boxed) ==="
  let vecPs = V.fromList ps
  timeAction "First solution (Boxed Vector)" $ print (take 1 $ solve vecPs t)
  timeAction "All solutions (Boxed Vector)" $ do
    let sols = solve vecPs t
    putStrLn $ "Total count: " ++ show (length sols)

  putStrLn "\n=== Target 765 (Data.Vector.Unboxed Int) ==="
  let uVecInt = U.fromList nums
  let Just tInt = mkPositive 765
  timeAction "First solution (Unboxed Vector)" $ print (take 1 $ solve (map Positive (U.toList uVecInt)) tInt)
  timeAction "All solutions (Unboxed Vector)" $ do
    let sols = solve (map Positive (U.toList uVecInt)) tInt
    putStrLn $ "Total count: " ++ show (length sols)
