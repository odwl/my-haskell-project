{-|
Module      : CountDown
Description : Solver for the Countdown Numbers Game with generic sequence abstraction.
Reference   : "The Countdown Problem" by Graham Hutton (Journal of Functional Programming, 2002)
              URL: https://www.cs.nott.ac.uk/~pszgmh/countdown.pdf

The Countdown Numbers Game:
Given a sequence of positive integers (e.g. [1, 3, 7, 10, 25, 50]) and a target 
positive integer (e.g. 765), construct arithmetic expressions using basic operators 
(+, -, *, /) that evaluate precisely to the target.

Rules & Invariants:
1. Each number from the input sequence can be used at most once.
2. Every intermediate and final result must be a Positive integer (> 0).
3. Subtraction (x - y) requires x > y (no zero or negative intermediate values).
4. Division (x / y) requires exact integer division (x `mod` y == 0) and y /= 1.
5. Algebraic Symmetry Pruning (Hutton's Optimization):
   - Add: x + y is only generated when x <= y (eliminates commutativity duplicates).
   - Mul: x * y is only generated when x <= y and x /= 1 (eliminates identity and commutativity duplicates).
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module CountDown (Op(..), allOps, apply, valid, Positive, mkPositive, unPositive, (.+), (.*), Expr(Val), mkApp, one, values, eval, choices, split, combine, exprs, solve, solutions, main) where

import Data.List (subsequences, permutations)
import Data.Maybe (maybeToList)
import Control.Monad (guard)
import Control.Category ((>>>))
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.MonoTraversable (olength, onull, headEx, Element, otoList, oproduct)
import Data.Sequences (IsSequence, splitAt, Index, fromList)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U


--------------------------------------------------------------------------------
-- Positive Numbers & Domain Logic
--------------------------------------------------------------------------------

-- Note: Deriving Num is a pragmatic convenience for container functions like `oproduct`.
-- Mathematically, Positive is a Semiring (elements > 0) and does not support total 
-- subtraction or zero, but deriving Num allows built-in oproduct/sum utilities.
newtype Positive = Positive { unPositive :: Int }
  deriving stock (Eq, Ord)
  deriving newtype (Num)

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
values e = go e []
  where
    go (Val x) acc = x : acc
    go (App _ l r _) acc = go l (go r acc)

eval :: Expr -> Positive
eval (Val n) = n
eval (App _ _ _ val) = val


--------------------------------------------------------------------------------
-- Sequence Abstraction
--------------------------------------------------------------------------------

class Sequence seq where 
  sfromList :: [Positive] -> seq
  sToList   :: seq -> [Positive]
  sproduct  :: seq -> Positive
  snull     :: seq -> Bool
  slength   :: seq -> Int
  shead     :: seq -> Positive
  ssplitAt  :: Int -> seq -> (seq, seq)

-- Explicit fast-path instance for standard lists [Positive]
instance {-# OVERLAPPING #-} Sequence [Positive] where
  sfromList = id
  sToList   = id
  sproduct  = product
  snull     = null
  slength   = length
  shead (x:_) = x
  shead []    = error "Sequence.shead: empty list (invariant violation: guarded by slength == 1)"
  ssplitAt  = Prelude.splitAt
  {-# INLINE sfromList #-}
  {-# INLINE sToList #-}
  {-# INLINE sproduct #-}
  {-# INLINE snull #-}
  {-# INLINE slength #-}
  {-# INLINE shead #-}
  {-# INLINE ssplitAt #-}

-- IsSequence to Sequence adapter for all other containers.
-- Note: This instance requires UndecidableInstances due to the type-level constraints on seq
instance {-# OVERLAPPABLE #-} (IsSequence seq, Element seq ~ Positive, Index seq ~ Int) => Sequence seq where 
  sfromList = Data.Sequences.fromList
  sToList   = otoList
  sproduct  = oproduct
  snull     = onull
  slength   = olength
  shead     = headEx
  ssplitAt  = Data.Sequences.splitAt
  {-# INLINE sfromList #-}
  {-# INLINE sToList #-}
  {-# INLINE sproduct #-}
  {-# INLINE snull #-}
  {-# INLINE slength #-}
  {-# INLINE shead #-}
  {-# INLINE ssplitAt #-}

--------------------------------------------------------------------------------
-- Brute Force Search
--------------------------------------------------------------------------------

-- `choices` returns all possible permutations of all possible sub-sequences of a given sequence.
-- This represents picking any subset of numbers in any possible order.
choices :: Sequence seq => seq -> [seq]
choices = sToList
      >>> subsequences
      >>> concatMap permutations
      >>> map sfromList

-- `split` generates all possible ways to divide a sequence into two non-empty halves 
-- without changing the order of the elements.
split :: Sequence seq => seq -> [(seq, seq)]
split xs = do
  i <- [1 .. slength xs - 1]
  pure (ssplitAt i xs)

-- `combine` takes a left and right expression and attempts to join them 
-- using every possible operator.
combine :: Expr -> Expr -> [Expr]
combine l r = do
  op <- allOps
  maybeToList (mkApp op l r)

-- `exprs` generates every perfectly valid mathematical tree that can be 
-- formed from a given sequence of numbers.
exprs :: Sequence seq => seq -> [Expr]
exprs ns
  | snull ns        = []
  | slength ns == 1 = [Val (shead ns)]
  | otherwise        = do 
      (ls, rs) <- split ns 
      l <- exprs ls
      r <- exprs rs
      combine l r

solve :: Sequence seq => seq -> Positive -> [Expr]
solve ns target = do 
  choice <- choices ns 
  guard (sproduct choice >= target)
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
  let Just t765 = mkPositive 765
  let Just t831 = mkPositive 831
  let Just t25  = mkPositive 25

  putStrLn "=== Target 25 (Check 1 Direct Single-Number Match) ==="
  timeAction "List (Direct Match 25)" $ print (take 1 $ solve ps t25)
  timeAction "Unboxed Vector (Direct Match 25)" $ do
    let uVecInt = U.fromList nums
    print (take 1 $ solve (map Positive (U.toList uVecInt)) t25)

  putStrLn "\n=== Target 765 (49 Solutions) ==="
  timeAction "List (First)" $ print (take 1 $ solve ps t765)
  timeAction "List (All 49)" $ print (length $ solve ps t765)
  let vecPs = V.fromList ps
  timeAction "Boxed Vector (First)" $ print (take 1 $ solve vecPs t765)
  timeAction "Boxed Vector (All 49)" $ print (length $ solve vecPs t765)
  let uVecInt = U.fromList nums
  timeAction "Unboxed Vector (First)" $ print (take 1 $ solve (map Positive (U.toList uVecInt)) t765)
  timeAction "Unboxed Vector (All 49)" $ print (length $ solve (map Positive (U.toList uVecInt)) t765)

  putStrLn "\n=== Target 831 (No Solutions / Impossible) ==="
  timeAction "List (Impossible 831)" $ print (length $ solve ps t831)
  timeAction "Boxed Vector (Impossible 831)" $ print (length $ solve vecPs t831)
  timeAction "Unboxed Vector (Impossible 831)" $ print (length $ solve (map Positive (U.toList uVecInt)) t831)

  let nums7 = [1, 3, 7, 10, 25, 50, 100]
  let Just ps7 = mapM mkPositive nums7
  let Just t952 = mkPositive 952
  let uVec7 = U.fromList nums7

  putStrLn "\n=== Target 952 with 7 Numbers (Large Search Space) ==="
  timeAction "List (First 952)" $ print (take 1 $ solve ps7 t952)
  timeAction "List (All 952)" $ print (length $ solve ps7 t952)
  timeAction "Unboxed Vector (All 952)" $ print (length $ solve (map Positive (U.toList uVec7)) t952)

  let nums10 = [1, 2, 3, 5, 7, 10, 15, 20, 25, 50]
  let Just ps10 = mapM mkPositive nums10
  let uVec10 = U.fromList nums10

  putStrLn "\n=== Target 952 with 10 Numbers (Huge Search Space) ==="
  timeAction "List (First 1 952)" $ print (take 1 $ solve ps10 t952)
  timeAction "Unboxed Vector (First 1 952)" $ print (take 1 $ solve (map Positive (U.toList uVec10)) t952)
  timeAction "List (First 10 952)" $ print (length $ take 10 $ solve ps10 t952)
  timeAction "Unboxed Vector (First 10 952)" $ print (length $ take 10 $ solve (map Positive (U.toList uVec10)) t952)
