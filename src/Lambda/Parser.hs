module Lambda.Parser (ReadP, Id, mkId, unId, num, binop, cmpop, identifier, aexp, expr, stmt, stmts, Stmt (..), Exp (..), AExp (..), BinOp (..), CmpOp (..)) where

import Data.Char (isAlpha, isAlphaNum, isAscii, isDigit)
import Text.ParserCombinators.ReadP (ReadP, char, munch, munch1, pfail, satisfy, skipSpaces, string, (<++))

identifier :: ReadP Id
identifier = lexeme $ do
  c <- satisfy (liftA2 (&&) isAscii isAlpha)
  cs <- munch (liftA2 (&&) isAscii isAlphaNum)
  let name = c : cs
  if name `elem` ["not", "if", "then", "else", "fi", "while", "do", "done"] then pfail else return (Id c cs)

num :: ReadP Int
num = lexeme $ read <$> munch1 isDigit

binop :: ReadP BinOp
binop =
  lexeme $
    (Add <$ char '+')
      <++ (Sub <$ char '-')
      <++ (Mul <$ char '*')
      <++ (Div <$ char '/')

cmpop :: ReadP CmpOp
cmpop =
  lexeme $
    (Le <$ string "<=")
      <++ (Gt <$ char '>')
      <++ (Eq <$ string "==")
      <++ (Neq <$ string "!=")

aexp :: ReadP AExp
aexp = (Num <$> num) <++ (Var <$> identifier) <++ opP
  where
    opP =
      Op
        <$ lexeme (char '(')
        <*> aexp
        <*> binop
        <*> aexp
        <* lexeme (char ')')

expr :: ReadP Exp
expr = notP <++ ifP <++ aexpP
  where
    notP = Not <$> (keyword "not" *> expr)
    aexpP = do
      left <- aexp
      (Cmp left <$> cmpop <*> aexp) <++ pure (E_AExp left)
    ifP =
      If
        <$ keyword "if"
        <*> expr
        <* keyword "then"
        <*> expr
        <* keyword "else"
        <*> expr
        <* keyword "fi"

stmt :: ReadP Stmt
stmt = assignP <++ whileP
  where
    assignP =
      Assign
        <$> identifier
        <* lexeme (string ":=")
        <*> expr
    whileP =
      While
        <$ keyword "while"
        <*> expr
        <* keyword "do"
        <*> stmts
        <* keyword "done"

stmts :: ReadP [Stmt]
stmts = do
  s <- stmt
  (lexeme (string ";") *> ((s :) <$> stmts)) <++ return [s]

-- -- ==========================================
-- -- AST and ReadP Definitions
-- -- ==========================================

-- | `lexeme` runs a given parser, then consumes any trailing whitespace.
lexeme :: ReadP a -> ReadP a
lexeme p = p <* skipSpaces

-- | Extracts a reserved keyword.
keyword :: String -> ReadP ()
keyword k = lexeme $ do
  kw <- munch1 (liftA2 (&&) isAscii isAlphaNum)
  if kw == k then return () else pfail

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

data Stmt
  = While Exp [Stmt]
  | Assign Id Exp
  deriving (Show, Eq)

data Exp
  = If Exp Exp Exp
  | Cmp AExp CmpOp AExp
  | Not Exp
  | E_AExp AExp
  deriving (Show, Eq)

data AExp
  = Num Int
  | Var Id
  | Op AExp BinOp AExp
  deriving (Show, Eq)

data CmpOp = Le | Gt | Eq | Neq deriving (Show, Eq)

data BinOp = Add | Sub | Mul | Div deriving (Show, Eq)
