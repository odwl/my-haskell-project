module Parser (Parser (..), satisfy, term1, digit) where

import Data.Char

newtype Parser a = Parser
  { parse :: String -> Maybe (a, String)
  }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x : xs)
      | p x = Just (x, xs)
      | otherwise = Nothing

term1 :: Char -> Parser Char
term1 c = satisfy (== c)

digit :: Parser Char
digit = satisfy isDigit

-- term1 c = Parser f
--   where
--     f [] = Nothing
--     f (x : xs)
--       | x == c = Just (x, xs)
--       | otherwise = Nothing

--       1. Write a constructor for a parser: term1
-- , which takes a Char and returns a Parser Char which will
-- successfully parse the first character of the input if it is the character in question and will fail otherwise.
-- 2. Write a constructor for a parser: digit which will parse one digit (0 − 9) from the input.
-- 3. Write a constructor for a parser singularWhiteSpace which will parse a whitespace character (consider
-- only the four whitespace characters: space, tab, carriage return, and linefeed) from the input.
-- 4. Write a constructor for a parser endOfStream: which succeeds if the string is empty and fails otherwise.
