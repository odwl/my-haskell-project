module Exercism.Clock (addDelta, fromHourMin, toString, Clock) where

import Text.Printf (printf)

newtype Hour = Hour {hour :: Int} deriving (Eq, Show)

newtype Minute = Minute {minute :: Int} deriving (Eq, Show)

data Clock = Clock Hour Minute
  deriving (Eq, Show)

fromHourMin :: Int -> Int -> Clock
fromHourMin h m =
  let totalMinutes = h * 60 + m
      normalizedTotal = totalMinutes `mod` (24 * 60)
      (h', m') = normalizedTotal `divMod` 60
   in Clock (Hour h') (Minute m')

toString :: Clock -> String
toString (Clock (Hour h) (Minute m)) = printf "%02d:%02d" h m

addDelta :: Int -> Int -> Clock -> Clock
addDelta dh dm (Clock (Hour h) (Minute m)) = fromHourMin (h + dh) (m + dm)
