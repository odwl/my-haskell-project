module Exercism.Bob (hey) where

import Data.Char

-- import Data.Char (isAlpha, isUpper)

-- Bob is a lackadaisical teenager. He responds to what you say to him.
-- He answers 'Sure.' if you ask him a question and the question is not yelling.
-- He answers 'Whoa, chill out!' if you yell at him.
-- He answers 'Calm down, I know what I'm doing!' if you yell a question at him.
-- He says 'Fine. Be that way!' if you address him without saying anything.
-- He answers 'Whatever.' to anything else.

hey :: String -> String
hey input
  | isSilent = "Fine. Be that way!"
  | isYelling && isQuestion = "Calm down, I know what I'm doing!"
  | isYelling = "Whoa, chill out!"
  | isQuestion = "Sure."
  | otherwise = "Whatever."
  where
    isSilent = all isSpace input
    isQuestion = last input == '?'
    -- isYelling = all isUpper $ filter isAlpha (init input)
    isYelling = any isAlpha input && all isUpper (filter isAlpha input)
