module Lambda.Interpreter
  ( Memory,
    Value,
    evalAExp,
    evalExp,
    evalStmt,
    evalStmts,
    emptyMemory,
  )
where

import Control.Monad (when)
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Lambda.Parser

type Memory = Map Id Value

emptyMemory :: Memory
emptyMemory = Map.empty

type InterpM = State Memory

evalAExp :: AExp -> InterpM Value
evalAExp (Lit v) = pure v
evalAExp (Var i) = gets (Map.findWithDefault (VInt 0) i)
evalAExp (Op left op right) = do
  v1 <- evalAExp left
  v2 <- evalAExp right
  return $ case (v1, v2) of
    (VInt n1, VInt n2) -> VInt $ case op of
      Add -> n1 + n2
      Sub -> n1 - n2
      Mul -> n1 * n2
      Div -> if n2 == 0 then 0 else n1 `div` n2
    _ -> VInt 0 -- Fallback for non-integer operations

evalExp :: Exp -> InterpM Value
evalExp (EAExp a) = evalAExp a
evalExp (Not e) = do
  v <- evalExp e
  return $ case v of
    VInt n -> if n == 0 then VInt 1 else VInt 0
    VBool b -> VBool (not b)
evalExp (Cmp left op right) = do
  v1 <- evalAExp left
  v2 <- evalAExp right
  let b = case (v1, v2) of
        (VInt n1, VInt n2) -> case op of
          Le -> n1 <= n2
          Gt -> n1 > n2
          Eq -> n1 == n2
          Neq -> n1 /= n2
        _ -> False
  return $ VInt (if b then 1 else 0)
evalExp (If cond thenE elseE) = do
  v <- evalExp cond
  let isTrue = case v of
        VInt n -> n /= 0
        VBool b -> b
  if isTrue
    then evalExp thenE
    else evalExp elseE

evalStmt :: Stmt -> InterpM ()
evalStmt (Assign i e) = do
  v <- evalExp e
  modify (Map.insert i v)
evalStmt (While cond body) = do
  v <- evalExp cond
  let continues = case v of
        VInt n -> n /= 0
        VBool b -> b
  when continues $ do
    evalStmts body
    evalStmt (While cond body)

evalStmts :: [Stmt] -> InterpM ()
evalStmts = mapM_ evalStmt
