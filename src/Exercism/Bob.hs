{-# LANGUAGE OverloadedStrings #-}

module Exercism.Bob (hey) where

import Data.Char (isAlpha, isUpper)
import qualified Data.Text as T

-- Bob is a lackadaisical teenager. He responds to what you say to him.
-- He answers 'Sure.' if you ask him a question and the question is not yelling.
-- He answers 'Whoa, chill out!' if you yell at him.
-- He answers 'Calm down, I know what I'm doing!' if you yell a question at him.
-- He says 'Fine. Be that way!' if you address him without saying anything.
-- He answers 'Whatever.' to anything else.

hey :: String -> String
hey input = heyText (T.pack input)

heyText :: T.Text -> String
heyText input
  | isSilent = "Fine. Be that way!"
  | isYelling && isQuestion = "Calm down, I know what I'm doing!"
  | isYelling = "Whoa, chill out!"
  | isQuestion = "Sure."
  | otherwise = "Whatever."
  where
    trimmed = T.strip input
    isSilent = T.null trimmed
    isQuestion = T.isSuffixOf "?" trimmed
    letters = T.filter isAlpha trimmed
    isYelling = not (T.null letters) && T.all isUpper letters