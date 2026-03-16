{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.List (sortBy)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ord (comparing)
import qualified Data.Text as T
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
-- Import the API module

import Network.HTTP.Req
import SerpApi
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- Swiss Destinations
--------------------------------------------------------------------------------

-- | European towns that have direct flights from Zurich with Swiss International Air Lines.
swissEuropeanDestinationsFromZurich :: [(String, String)]
swissEuropeanDestinationsFromZurich =
  [ ("Amsterdam", "AMS")
  -- ("Athens", "ATH"),
  -- ("Barcelona", "BCN"),
  -- ("Berlin", "BER"),
  -- ("Brussels", "BRU"),
  -- ("Budapest", "BUD"),
  -- ("Copenhagen", "CPH"),
  -- ("Dublin", "DUB"),
  -- ("Dubrovnik", "DBV"),
  -- ("Florence", "FLR"),
  -- ("Graz", "GRZ"),
  -- ("Istanbul", "IST"),
  -- ("Larnaca", "LCA"),
  -- ("Lisbon", "LIS"),
  -- ("London", "LHR"),
  -- ("Madrid", "MAD"),
  -- ("Manchester", "MAN"),
  -- ("Milan", "MXP"),
  -- ("Nice", "NCE"),
  -- ("Oslo", "OSL"),
  -- ("Palma de Mallorca", "PMI"),
  -- ("Paris", "CDG"),
  -- ("Prague", "PRG"),
  -- ("Pristina", "PRN"),
  -- ("Rome", "FCO"),
  -- ("Sarajevo", "SJJ"),
  -- ("Sofia", "SOF"),
  -- ("Stockholm", "ARN"),
  -- ("Tirana", "TIA"),
  -- ("Venice", "VCE"),
  -- ("Vienna", "VIE"),
  -- ("Warsaw", "WAW")
  ]

--------------------------------------------------------------------------------
-- Logic and UI
--------------------------------------------------------------------------------

data RowData = RowData
  { rdDest :: String,
    rdPrice :: Int,
    rdStart :: String,
    rdEnd :: String,
    rdDur :: Int
  }
  deriving (Show)

-- | Business logic to find best direct return flights
fetchBestFlightsTable :: String -> IO ()
fetchBestFlightsTable key = do
  now <- getCurrentTime
  let days = 24 * 60 * 60
  let todayStr = formatTime defaultTimeLocale "%Y-%m-%d" (addUTCTime (7 * days) now)
  let nextWeekStr = formatTime defaultTimeLocale "%Y-%m-%d" (addUTCTime (14 * days) now)

  putStrLn $ "Finding best direct return flights from Zurich..."
  putStrLn $ "Outbound: " ++ todayStr ++ " | Return: " ++ nextWeekStr
  putStrLn "Querying European destinations (this may take a moment)..."

  results <- runReq defaultHttpConfig $ do
    forM swissEuropeanDestinationsFromZurich $ \(cityName, iataCode) -> do
      liftIO $ putStrLn $ " - Querying " ++ cityName ++ " (" ++ iataCode ++ ")..."

      let opts =
            FlightSearchOptions
              { departureId = "ZRH",
                arrivalId = T.pack iataCode,
                outboundDate = T.pack todayStr,
                returnDate = Just (T.pack nextWeekStr),
                travelType = "1", -- Round trip
                stops = Just "1", -- Nonstop
                currency = "CHF",
                apiKey = T.pack key
              }

      respData <- searchFlights opts

      let allFs = maybe [] id (best_flights respData) ++ maybe [] id (other_flights respData)

      if null allFs
        then return Nothing
        else do
          let best = head (sortBy (comparing price) allFs)
          return $ Just (cityName, best)

  let rows = mapMaybe toRowData (catMaybes results)
      sortedRows = sortBy (comparing rdPrice) rows

  putStrLn "\n========================================================================================"
  printf "%-20s | %-16s | %-16s | %-10s | %-10s\n" ("Destination" :: String) ("Start Time" :: String) ("End Time" :: String) ("Price CHF" :: String) ("Duration m" :: String)
  putStrLn "----------------------------------------------------------------------------------------"
  forM_ sortedRows $ \r -> do
    printf "%-20s | %-16s | %-16s | %-10d | %-10d\n" (rdDest r) (rdStart r) (rdEnd r) (rdPrice r) (rdDur r)
  putStrLn "========================================================================================\n"
  where
    toRowData :: (String, BestFlight) -> Maybe RowData
    toRowData (dest, bf) = do
      p <- price bf
      fs <- flights bf
      f <- if null fs then Nothing else Just (head fs)
      dep <- departure_airport f
      arr <- arrival_airport f
      st <- time dep
      et <- time arr
      d <- duration f
      return $ RowData dest p (T.unpack st) (T.unpack et) d

main :: IO ()
main = do
  mKey <- lookupEnv "SERPAPI_API_KEY"
  case mKey of
    Nothing -> do
      putStrLn "Error: SERPAPI_API_KEY environment variable not set."
      exitFailure
    Just key -> fetchBestFlightsTable key
