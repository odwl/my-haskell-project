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
    Value (..),
  )
where

-- Inspired by: https://proglang.informatik.uni-freiburg.de/teaching/functional-programming/2019/ex/ex5.pdf

import Control.Monad (guard)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Char (isAlpha, isAlphaNum, isAscii)
import Data.Void (Void)
import Text.Megaparsec (Parsec, choice, many, notFollowedBy, optional, satisfy, sepBy1, try, (<|>))
import Text.Megaparsec.Char (alphaNumChar, letterChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

-- ==========================================
-- Types Definitions
-- ==========================================

type Parser = Parsec Void String

reservedWords :: [String]
reservedWords = ["not", "if", "then", "else", "fi", "while", "do", "done", "true", "false"]

isReservedWord :: String -> Bool
isReservedWord = (`elem` reservedWords)

data Id = Id Char String
  deriving (Show, Eq, Ord)

-- | Smart constructor that explicitly takes the starting Char and the rest of the String.
mkId :: Char -> String -> Maybe Id
mkId c cs
  | isAlpha c && all isAlphaNum cs = Just (Id c cs)
  | otherwise = Nothing

data CmpOp = Le | Gt | Eq | Neq deriving (Show, Eq)

data BinOp = Add | Sub | Mul | Div deriving (Show, Eq)

data Value = VInt Int | VBool Bool
  deriving (Show, Eq)

data AExp
  = Lit Value
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
  c <- letterChar
  cs <- many alphaNumChar
  let name = c : cs
  guard (not (isReservedWord name))
  pure (Id c cs)

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

literal :: Parser Value
literal =
  choice
    [ VInt <$> num,
      VBool True <$ keyword "true",
      VBool False <$ keyword "false"
    ]

aTerm :: Parser AExp
aTerm =
  choice
    [ parens aexp,
      Lit <$> literal,
      Var <$> identifier
    ]

parens :: Parser a -> Parser a
parens p = symbol "(" *> p <* symbol ")"

aOperators :: [[Operator Parser AExp]]
aOperators =
  [ [ InfixL ((`Op` Mul) <$ symbol "*"),
      InfixL ((`Op` Div) <$ symbol "/")
    ],
    [ InfixL ((`Op` Add) <$ symbol "+"),
      InfixL ((`Op` Sub) <$ symbol "-")
    ]
  ]

aexp :: Parser AExp
aexp = makeExprParser aTerm aOperators

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

-- -- Helper from Text.Megaparsec
-- notFollowedBy :: Parser a -> Parser ()
-- notFollowedBy p = do
--   res <- optional (try p)
--   case res of
--     Nothing -> pure ()
--     Just _ -> fail "keyword followed by alphanumeric character"
