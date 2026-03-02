module Lambda.Parser (ReadP, Id, mkId, unId, num, binop, cmpop, identifier, aexp, expr, stmt, stmts, Stmt (..), Stmts (..), Exp (..), AExp (..), BinOp(..), CmpOp(..)) where

import Control.Applicative (Alternative(..))
import Data.Char (isAlpha, isAlphaNum, isAscii, isDigit, isSpace)
import Text.ParserCombinators.ReadP (ReadP, char, munch, munch1, satisfy, skipSpaces, string, (<++))

identifier :: ReadP Id
identifier = lexeme $ do
  c <- satisfy (liftA2 (&&) isAscii isAlpha)
  cs <- munch (liftA2 (&&) isAscii isAlphaNum)
  return (Id c cs)

num :: ReadP Int
num = lexeme $ read <$> munch1 isDigit

binop :: ReadP BinOp
binop = lexeme $ 
  (char '+' *> pure Add) <|> 
  (char '-' *> pure Sub) <|> 
  (char '*' *> pure Mul) <|> 
  (char '/' *> pure Div)

cmpop :: ReadP CmpOp
cmpop = lexeme $
  (string "<=" *> pure Le) <|>
  (char '>' *> pure Gt) <|>
  (string "==" *> pure Eq) <|>
  (string "!=" *> pure Neq)

aexp :: ReadP AExp
aexp = (Num <$> num) <|> (Var <$> identifier)

expr :: ReadP Exp
expr = E_AExp <$> aexp

stmts :: ReadP Stmts
stmts = do
  s <- stmt
  let sequenceSemicolon = fmap (Seq s) (lexeme (string ";") *> stmts)
  sequenceSemicolon <++ return (Single s)

stmt :: ReadP Stmt
stmt = do
  v <- identifier
  _ <- lexeme (string ":=")
  e <- expr
  return (Assign v e)

-- -- ==========================================
-- -- AST and ReadP Definitions
-- -- ==========================================

-- | `lexeme` runs a given parser, then consumes any trailing whitespace.
lexeme :: ReadP a -> ReadP a
lexeme p = p <* skipSpaces

-- -- | Spaces parser
-- spaces :: ReadP ()
-- spaces = many whiteSpace *> pure ()

-- -- | Token parser
-- token :: ReadP a -> ReadP a
-- token p = p <* spaces

-- -- | Symbol parser
-- symbol :: String -> ReadP String
-- symbol s = token (parseString s)

-- | Types
data Id = Id Char String
  deriving (Show, Eq)

unId :: Id -> String
unId (Id c cs) = c : cs

-- | Smart constructor that explicitly takes the starting Char and the rest of the String.
mkId :: Char -> String -> Maybe Id
mkId c cs
  | isAscii c && isAlpha c && all (\x -> isAscii x && isAlphaNum x) cs = Just (Id c cs)
  | otherwise = Nothing

data Stmts
  = Seq Stmt Stmts
  | Single Stmt
  deriving (Show, Eq)

data Stmt
  = --   While Exp [Stmt]
    Assign Id Exp
  deriving (Show, Eq)

data Exp
  = -- If Exp Exp Exp

    -- | Cmp AExp CmpOp AExp
    -- | Not Exp
    -- |
    E_AExp AExp
  deriving (Show, Eq)

data AExp
  = Num Int | Var Id
  deriving( Show, Eq)

-- \| Op AExp BinOp AExp

data CmpOp = Le | Gt | Eq | Neq deriving (Show, Eq)
data BinOp = Add | Sub | Mul | Div deriving (Show, Eq)

-- -- | ReadPs

-- parseStmts :: ReadP [Stmt]
-- parseStmts = do
--   s <- parseStmt
--   (symbol ";" >> ((s :) <$> parseStmts)) <|> pure [s]

-- parseStmt :: ReadP Stmt
-- parseStmt = parseWhile <|> parseAssign

-- parseWhile :: ReadP Stmt
-- parseWhile = do
--   symbol "while"
--   cond <- parseExp
--   symbol "do"
--   body <- parseStmts
--   symbol "done"
--   return (While cond body)

-- parseAssign :: ReadP Stmt
-- parseAssign = do
--   v <- identifier
--   symbol ":="
--   e <- parseExp
--   return (Assign v e)

-- parseExp :: ReadP Exp
-- parseExp = parseIf <|> parseNot <|> parseCmpOrAExp

-- parseIf :: ReadP Exp
-- parseIf = do
--   symbol "if"
--   c <- parseExp
--   symbol "then"
--   t <- parseExp
--   symbol "else"
--   e <- parseExp
--   symbol "fi"
--   return (If c t e)

-- parseCmpOrAExp :: ReadP Exp
-- parseCmpOrAExp = do
--   a1 <- parseAExp
--   (do
--      op <- parserCmpOp
--      a2 <- parseAExp
--      return (Cmp a1 op a2)
--    ) <|> return (E_AExp a1)

-- parseNot :: ReadP Exp
-- parseNot = do
--   symbol "not"
--   Not <$> parseExp

-- parseAExp :: ReadP AExp
-- parseAExp = parseNum <|> parseVar <|> parseBinOp

-- parseNum :: ReadP AExp
-- parseNum = Num . read <$> token (some digit)

-- parseVar :: ReadP AExp
-- parseVar = Var <$> identifier

-- parseBinOp :: ReadP AExp
-- parseBinOp = do
--   symbol "("
--   a1 <- parseAExp
--   op <- parserBinOpSym
--   a2 <- parseAExp
--   symbol ")"
--   return (Op a1 op a2)

-- parserCmpOp :: ReadP CmpOp
-- parserCmpOp = (symbol "<=" *> pure Le)
--           <|> (symbol ">" *> pure Gt)
--           <|> (symbol "==" *> pure Eq)
--           <|> (symbol "!=" *> pure Neq)

-- parserBinOpSym :: ReadP BinOp
-- parserBinOpSym = (symbol "+" *> pure Add)
--              <|> (symbol "-" *> pure Sub)
--              <|> (symbol "*" *> pure Mul)
--              <|> (symbol "/" *> pure Div)

-- identifier :: ReadP String
-- identifier = token $ do
--   c <- satisfy (\c -> isAscii c && isAlpha c)
--   cs <- many (satisfy (\c -> isAscii c && isAlphaNum c))
--   return (c:cs)
