{-# LANGUAGE OverloadedStrings #-}

module Exercism.Bob (hey, ResponseType (..), responseString) where

import Data.Char (isAlpha, isUpper)
import qualified Data.Text as T

-- Bob is a lackadaisical teenager. He responds to what you say to him.
-- He answers 'Sure.' if you ask him a question and the question is not yelling.
-- He answers 'Whoa, chill out!' if you yell at him.
-- He answers 'Calm down, I know what I'm doing!' if you yell a question at him.
-- He says 'Fine. Be that way!' if you address him without saying anything.
-- He answers 'Whatever.' to anything else.

data ResponseType
  = Fine
  | CalmDown
  | Whoa
  | Sure
  | Whatever
  deriving (Show, Eq, Enum, Bounded)

responseString :: ResponseType -> String
responseString Fine = "Fine. Be that way!"
responseString CalmDown = "Calm down, I know what I'm doing!"
responseString Whoa = "Whoa, chill out!"
responseString Sure = "Sure."
responseString Whatever = "Whatever."

hey :: String -> String
hey input = heyText (T.pack input)

heyText :: T.Text -> String
heyText input
  | isSilent = responseString Fine
  | isYelling && isAsking = responseString CalmDown
  | isYelling = responseString Whoa
  | isAsking = responseString Sure
  | otherwise = responseString Whatever
  where
    trimmed = T.strip input
    isSilent = T.null trimmed
    letters = T.filter isAlpha trimmed
    isYelling = not (T.null letters) && T.all isUpper letters
    isAsking = T.isSuffixOf "?" trimmed