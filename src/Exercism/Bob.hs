{-# LANGUAGE OverloadedStrings #-}

module Exercism.Bob (responseFor, ResponseType (..), responseTxt) where

import Data.Char (isAlpha, isUpper)
import qualified Data.Text as T

data ResponseType
  = Fine
  | CalmDown
  | Whoa
  | Sure
  | Whatever
  deriving (Show, Eq, Enum, Bounded)

responseTxt :: ResponseType -> T.Text
responseTxt Fine = "Fine. Be that way!"
responseTxt CalmDown = "Calm down, I know what I'm doing!"
responseTxt Whoa = "Whoa, chill out!"
responseTxt Sure = "Sure."
responseTxt Whatever = "Whatever."

responseFor :: T.Text -> T.Text
responseFor input
  | isSilent = responseTxt Fine
  | isYelling && isAsking = responseTxt CalmDown
  | isYelling = responseTxt Whoa
  | isAsking = responseTxt Sure
  | otherwise = responseTxt Whatever
  where
    trimmed = T.strip input
    isSilent = T.null trimmed
    letters = T.filter isAlpha trimmed
    isYelling = not (T.null letters) && T.all isUpper letters
    isAsking = T.isSuffixOf "?" trimmed