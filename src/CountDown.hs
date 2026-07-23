{-# LANGUAGE GADTs #-}
module CountDown (Op(..), allOps, apply, valid, Positive, mkPositive, unPositive, (.+), (.*), Expr(Val), mkApp, one, values, eval, choices, split, combine, exprs, solve, solutions, main) where

import Data.List (subsequences, permutations, (\\))
import Data.Bifunctor (first)
import Data.Maybe (maybeToList)
import Control.Monad (guard)


--------------------------------------------------------------------------------
-- Positive Numbers & Domain Logic
--------------------------------------------------------------------------------

data Positive where
  Positive :: { unPositive :: Int } -> Positive
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
allOps = [minBound .. maxBound]

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

toFun :: Op -> (Int -> Int -> Int)
toFun Add = (+)
toFun Sub = (-)
toFun Mul = (*)
toFun Div = div

apply :: Op -> Positive -> Positive -> Positive
apply op (Positive x) (Positive y) = Positive (toFun op x y)

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

-- `choices` returns all possible permutations of all possible sub-lists of a given list.
-- This represents picking any subset of numbers in any possible order.
choices :: [a] -> [[a]]
choices = concatMap permutations . subsequences

-- `split` generates all possible ways to divide a list into two non-empty halves 
-- without changing the order of the elements.
split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : map (first (x:)) (split xs)

-- `combine` takes a left and right expression and attempts to join them 
-- using every possible operator.
combine :: Expr -> Expr -> [Expr]
combine l r = do
  op <- allOps
  maybeToList (mkApp op l r)

-- `exprs` generates every perfectly valid mathematical tree that can be 
-- formed from a given list of numbers.
exprs :: [Positive] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = do 
  (ls, rs) <- split ns 
  l <- exprs ls
  r <- exprs rs
  combine l r

solve :: [Positive] -> Positive -> [Expr]
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

main :: IO ()
main = print (solutions [1, 3, 7, 10, 25, 50] 765)
