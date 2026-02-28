module Lambda.Parser (Parser (..), satisfy, parseChar, parseString, digit, whiteSpace, endOfStream, parserInt, parseTwoChars) where

import Control.Applicative (Alternative (..))
import Data.Bifunctor (first)
import Data.Char

newtype Parser a = Parser
  { parse :: String -> Maybe (a, String)
  }

instance Functor Parser where
  fmap f (Parser p) = Parser (fmap (first f) . p)

instance Applicative Parser where
  pure x = Parser (\s -> Just (x, s))
  (Parser pf) <*> (Parser px) = Parser $ \s -> do
    (f, s') <- pf s
    (x, s'') <- px s'
    Just (f x, s'')

instance Alternative Parser where
  empty = Parser (const Nothing)
  (Parser p1) <|> (Parser p2) = Parser $ \s -> p1 s <|> p2 s

instance Monad Parser where
  (>>=) (Parser p) f = Parser $ \s -> case p s of
    Nothing -> Nothing
    Just (x, s') -> parse (f x) s'

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f (x : xs) | p x = Just (x, xs)
    f _ = Nothing

parseChar :: Char -> Parser Char
parseChar c = satisfy (== c)

parseString :: String -> Parser String
parseString = traverse parseChar

digit :: Parser Char
digit = satisfy isDigit

whiteSpace :: Parser Char
whiteSpace = satisfy isSpace

endOfStream :: Parser ()
endOfStream = Parser f
  where
    f [] = Just ((), "")
    f _ = Nothing

parserInt :: Parser Int
parserInt = digitToInt <$> digit

parseTwoChars :: Parser String
parseTwoChars = parseString "ab"

-- parseTwoChars = (\a b -> [a, b]) <$> parseChar 'a' <*> parseChar 'b'
-- parseTwoChars = do
--   c1 <- term1 'a'
--   c2 <- term1 'b'
--   return [c1, c2]
