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

type Value = Int

type Memory = Map Id Value

emptyMemory :: Memory
emptyMemory = Map.empty

type InterpM = State Memory

evalAExp :: AExp -> InterpM Value
evalAExp (Num n) = return n
evalAExp (Var i) = gets (Map.findWithDefault 0 i)
evalAExp (Op left op right) = do
  v1 <- evalAExp left
  v2 <- evalAExp right
  return $ case op of
    Add -> v1 + v2
    Sub -> v1 - v2
    Mul -> v1 * v2
    Div -> if v2 == 0 then 0 else v1 `div` v2

evalExp :: Exp -> InterpM Value
evalExp (EAExp a) = evalAExp a
evalExp (Not e) = do
  v <- evalExp e
  return $ if v == 0 then 1 else 0
evalExp (Cmp left op right) = do
  v1 <- evalAExp left
  v2 <- evalAExp right
  let b = case op of
        Le -> v1 <= v2
        Gt -> v1 > v2
        Eq -> v1 == v2
        Neq -> v1 /= v2
  return $ if b then 1 else 0
evalExp (If cond thenE elseE) = do
  v <- evalExp cond
  if v /= 0
    then evalExp thenE
    else evalExp elseE

evalStmt :: Stmt -> InterpM ()
evalStmt (Assign i e) = do
  v <- evalExp e
  modify (Map.insert i v)
evalStmt (While cond body) = do
  v <- evalExp cond
  when (v /= 0) $ do
    evalStmts body
    evalStmt (While cond body)

evalStmts :: [Stmt] -> InterpM ()
evalStmts = mapM_ evalStmt
