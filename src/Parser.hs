module Parser (Parser (..), satisfy, term1, digit, whiteSpace, endOfStream) where

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

whiteSpace :: Parser Char
whiteSpace = satisfy isSpace

endOfStream :: Parser ()
endOfStream = Parser f
  where
    f [] = Just ((), "")
    f _ = Nothing