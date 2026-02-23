module Parser (Parser (..), satisfy) where

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
