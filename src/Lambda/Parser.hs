module Lambda.Parser (ReadP, digit, whiteSpace, endOfStream, parseTwoChars, parseDigits, identifier) where

import Data.Char (isAlpha, isAlphaNum, isAscii, isDigit, isSpace)
import Text.ParserCombinators.ReadP (ReadP, eof, munch, munch1, satisfy, string)

digit :: ReadP Char
digit = satisfy isDigit

whiteSpace :: ReadP Char
whiteSpace = satisfy isSpace

endOfStream :: ReadP ()
endOfStream = eof

-- Specific parsers from the original file

parseTwoChars :: ReadP String
parseTwoChars = string "ab"

parseDigits :: ReadP Int
parseDigits = read <$> munch1 isDigit

(.&&.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
f .&&. g = \x -> f x && g x

infixr 3 .&&.

-- (.||.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
-- f .||. g = \x -> f x || g x

-- infixr 2 .||.

identifier :: ReadP String
identifier = do
  c <- satisfy (isAscii .&&. isAlpha)
  cs <- munch (isAscii .&&. isAlphaNum)
  return (c : cs)

-- -- ==========================================
-- -- AST and ReadP Definitions
-- -- ==========================================

-- -- | Spaces parser
-- spaces :: ReadP ()
-- spaces = many whiteSpace *> pure ()

-- -- | Token parser
-- token :: ReadP a -> ReadP a
-- token p = p <* spaces

-- -- | Symbol parser
-- symbol :: String -> ReadP String
-- symbol s = token (parseString s)

-- -- | Types
-- type Id = String

-- data Stmt
--   = While Exp [Stmt]
--   | Assign Id Exp
--   deriving (Show, Eq)

-- data Exp
--   = If Exp Exp Exp
--   | Cmp AExp CmpOp AExp
--   | Not Exp
--   | E_AExp AExp
--   deriving (Show, Eq)

-- data AExp
--   = Num Int
--   | Var Id
--   | Op AExp BinOp AExp
--   deriving (Show, Eq)

-- data CmpOp = Le | Gt | Eq | Neq deriving (Show, Eq)

-- data BinOp = Add | Sub | Mul | Div deriving (Show, Eq)

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
