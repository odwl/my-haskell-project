module Lambda.Parser (ReadP, Id, mkId, num, binop, cmpop, identifier, aexp, expr, stmt, stmts, Stmt (..), Exp (..), AExp (..), BinOp (..), CmpOp (..), reservedWords, isReservedWord) where

import Control.Monad (guard)
import Data.Char (isAlpha, isAlphaNum, isAscii, isDigit)
import Text.ParserCombinators.ReadP (ReadP, char, choice, munch, munch1, satisfy, skipSpaces, string, (<++))

-- ==========================================
-- Types Definitions
-- ==========================================

reservedWords :: [String]
reservedWords = ["not", "if", "then", "else", "fi", "while", "do", "done"]

isReservedWord :: String -> Bool
isReservedWord = (`elem` reservedWords)

data Id = Id Char String
  deriving (Show, Eq)

-- | Smart constructor that explicitly takes the starting Char and the rest of the String.
mkId :: Char -> String -> Maybe Id
mkId c cs
  | isAscii c && isAlpha c && all (\x -> isAscii x && isAlphaNum x) cs = Just (Id c cs)
  | otherwise = Nothing

data CmpOp = Le | Gt | Eq | Neq deriving (Show, Eq)

data BinOp = Add | Sub | Mul | Div deriving (Show, Eq)

data AExp
  = Num Int
  | Var Id
  | Op AExp BinOp AExp
  deriving (Show, Eq)

data Exp
  = If Exp Exp Exp
  | Cmp AExp CmpOp AExp
  | Not Exp
  | EAExp AExp
  deriving (Show, Eq)

data Stmt
  = While Exp [Stmt]
  | Assign Id Exp
  deriving (Show, Eq)

-- ==========================================
-- Parser Definitions
-- ==========================================

identifier :: ReadP Id
identifier = lexeme $ do
  c <- satisfy (liftA2 (&&) isAscii isAlpha)
  cs <- munch (liftA2 (&&) isAscii isAlphaNum)
  let name = c : cs
  guard (not (isReservedWord name))
  return (Id c cs)

binop :: ReadP BinOp
binop =
  lexeme $
    choice
      [ Add <$ char '+',
        Sub <$ char '-',
        Mul <$ char '*',
        Div <$ char '/'
      ]

cmpop :: ReadP CmpOp
cmpop =
  lexeme $
    choice
      [ Le <$ string "<=",
        Gt <$ char '>',
        Eq <$ string "==",
        Neq <$ string "!="
      ]

num :: ReadP Int
num = lexeme $ read <$> munch1 isDigit

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
      (Cmp left <$> cmpop <*> aexp) <++ pure (EAExp left)
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
stmts = (:) <$> stmt <*> ((lexeme (char ';') *> stmts) <++ pure [])

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
  guard (kw == k)
