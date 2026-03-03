module Lambda.MiniWhile where

-- Inspired by: https://proglang.informatik.uni-freiburg.de/teaching/functional-programming/2019/ex/ex6.pdf

import Control.Monad.State
import Data.Char (isAlpha, isAlphaNum)
import Data.Foldable (traverse_)
import qualified Data.Map as Map
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- ==========================================
-- Types Definitions
-- ==========================================

data Program = Program [Stmt]
  deriving (Show, Eq)

data Stmt = Asgn Id Exp
  deriving (Show, Eq)

data Exp
  = Num Integer
  | Var Id
  | Add Exp Exp
  deriving (Show, Eq)

type Id = String

type Parser = Parsec Void String

-- ==========================================
-- Interpreter Implementation
-- ==========================================

type Memory = Map.Map Id Integer

type InterpM = Control.Monad.State.State Memory

-- | Evaluate an expression. Missing variables default to 0.
evalExp :: Exp -> InterpM Integer
evalExp (Num n) = return n
evalExp (Var i) = gets (Map.findWithDefault 0 i)
evalExp (Add e1 e2) = (+) <$> evalExp e1 <*> evalExp e2

-- | Execute a statement.
evalStmt :: Stmt -> InterpM ()
evalStmt (Asgn i e) =
  do
    val <- evalExp e
    modify (Map.insert i val)

-- | Run a sequence of statements and return the final memory state.
evalProgram :: Program -> Memory
evalProgram (Program stmts) = execState (traverse_ evalStmt stmts) Map.empty

-- ==========================================
-- Parser Helpers (Lexer)
-- ==========================================
-- ... (rest of the file)

-- | Space consumer
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

-- | Lexeme wrapper
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Symbol wrapper
symbol :: String -> Parser String
symbol = L.symbol sc

-- ==========================================
-- Parser Implementation
-- ==========================================

p_num :: Parser Integer
p_num = lexeme L.decimal

p_id :: Parser Id
p_id = lexeme $ try $ do
  c <- satisfy isAlpha
  cs <- many (satisfy isAlphaNum)
  return (c : cs)

p_exp :: Parser Exp
p_exp = (Num <$> p_num) <|> (Var <$> p_id) <|> parens
  where
    parens =
      Add
        <$ symbol "("
        <*> p_exp
        <* symbol "+"
        <*> p_exp
        <* symbol ")"

p_stmt :: Parser Stmt
p_stmt = Asgn <$> p_id <* symbol ":=" <*> p_exp

p_program :: Parser Program
p_program = Program <$> sepBy1 p_stmt (symbol ";")

-- ==========================================
-- API
-- ==========================================

parseString :: String -> Maybe Program
parseString s = parseMaybe (sc *> p_program <* eof) s
