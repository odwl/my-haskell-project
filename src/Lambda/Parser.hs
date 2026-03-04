module Lambda.Parser
  ( Parser,
    Id,
    mkId,
    num,
    binop,
    cmpop,
    identifier,
    aexp,
    expr,
    stmt,
    stmts,
    Stmt (..),
    Exp (..),
    AExp (..),
    BinOp (..),
    CmpOp (..),
    reservedWords,
    isReservedWord,
    isAsciiAlpha,
    isAsciiAlphaNum,
  )
where

-- Inspired by: https://proglang.informatik.uni-freiburg.de/teaching/functional-programming/2019/ex/ex5.pdf

import Control.Monad (guard)
import Data.Char (isAlpha, isAlphaNum, isAscii)
import Data.Void (Void)
import Text.Megaparsec (Parsec, choice, many, satisfy, sepBy1, try, (<|>))
import Text.Megaparsec.Char (space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

-- ==========================================
-- Types Definitions
-- ==========================================

type Parser = Parsec Void String

reservedWords :: [String]
reservedWords = ["not", "if", "then", "else", "fi", "while", "do", "done"]

isReservedWord :: String -> Bool
isReservedWord = (`elem` reservedWords)

data Id = Id Char String
  deriving (Show, Eq)

-- | Predicates for identifier characters.
isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = isAscii c && isAlpha c

isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = isAscii c && isAlphaNum c

-- | Smart constructor that explicitly takes the starting Char and the rest of the String.
mkId :: Char -> String -> Maybe Id
mkId c cs
  | isAsciiAlpha c && all isAsciiAlphaNum cs = Just (Id c cs)
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

-- | `sc` (Space Consumer) handles whitespace and comments.
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

-- | `lexeme` runs a given parser, then consumes any trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | `symbol` parses a specific string and consumes trailing whitespace.
symbol :: String -> Parser String
symbol = L.symbol sc

identifier :: Parser Id
identifier = lexeme $ try $ do
  c <- satisfy isAsciiAlpha
  cs <- many (satisfy isAsciiAlphaNum)
  let name = c : cs
  guard (not (isReservedWord name))
  return (Id c cs)

binop :: Parser BinOp
binop =
  choice
    [ Add <$ symbol "+",
      Sub <$ symbol "-",
      Mul <$ symbol "*",
      Div <$ symbol "/"
    ]

cmpop :: Parser CmpOp
cmpop =
  choice
    [ Le <$ symbol "<=",
      Gt <$ symbol ">",
      Eq <$ symbol "==",
      Neq <$ symbol "!="
    ]

num :: Parser Int
num = lexeme L.decimal

aexp :: Parser AExp
aexp = opP <|> (Num <$> num) <|> (Var <$> identifier)
  where
    opP = do
      _ <- symbol "("
      left <- aexp
      op <- binop
      right <- aexp
      _ <- symbol ")"
      return (Op left op right)

expr :: Parser Exp
expr = notP <|> ifP <|> aexpBased
  where
    notP = Not <$> (keyword "not" *> expr)
    ifP =
      If
        <$ keyword "if"
        <*> expr
        <* keyword "then"
        <*> expr
        <* keyword "else"
        <*> expr
        <* keyword "fi"
    aexpBased = do
      left <- aexp
      choice
        [ Cmp left <$> cmpop <*> aexp,
          pure (EAExp left)
        ]

stmt :: Parser Stmt
stmt = assignP <|> whileP
  where
    assignP =
      Assign
        <$> identifier
        <* symbol ":="
        <*> expr
    whileP =
      While
        <$ keyword "while"
        <*> expr
        <* keyword "do"
        <*> stmts
        <* keyword "done"

stmts :: Parser [Stmt]
stmts = sepBy1 stmt (symbol ";")

-- | Extracts a reserved keyword.
keyword :: String -> Parser String
keyword k = lexeme (string k <* notFollowedBy (satisfy (liftA2 (&&) isAscii isAlphaNum)))

-- Helper from Text.Megaparsec
notFollowedBy :: Parser a -> Parser ()
notFollowedBy p = do
  res <- (Just <$> try p) <|> pure Nothing
  case res of
    Nothing -> return ()
    Just _ -> fail "keyword followed by alphanumeric character"
