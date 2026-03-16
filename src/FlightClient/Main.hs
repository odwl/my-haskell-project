{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import GHC.Generics
import Network.HTTP.Req
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import Data.Time.Clock (getCurrentTime, addUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Maybe (mapMaybe, catMaybes)
import Data.List (sortBy, intercalate)
import Data.Ord (comparing)
import Control.Monad (forM, forM_)
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- SerpApi Response Data Types
--------------------------------------------------------------------------------

data AirportInfo = AirportInfo
  { time :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON AirportInfo

data FlightDetail = FlightDetail
  { airline :: Maybe Text
  , flight_number :: Maybe Text
  , departure_airport :: Maybe AirportInfo
  , arrival_airport :: Maybe AirportInfo
  , duration :: Maybe Int
  } deriving (Show, Generic)

instance FromJSON FlightDetail

data BestFlight = BestFlight
  { flights :: Maybe [FlightDetail]
  , price :: Maybe Int
  } deriving (Show, Generic)

instance FromJSON BestFlight

data SerpApiResponse = SerpApiResponse
  { best_flights :: Maybe [BestFlight]
  , other_flights :: Maybe [BestFlight]
  } deriving (Show, Generic)

instance FromJSON SerpApiResponse

--------------------------------------------------------------------------------
-- Swiss Destinations
--------------------------------------------------------------------------------

-- | Returns European towns that have direct flights from Zurich with Swiss International Air Lines.
swissEuropeanDestinationsFromZurich :: [String]
swissEuropeanDestinationsFromZurich =
  [ "Amsterdam", "Athens", "Barcelona", "Berlin", "Brussels", "Budapest"
  , "Copenhagen", "Dublin", "Dubrovnik", "Florence", "Graz", "Istanbul"
  , "Larnaca", "Lisbon", "London", "Madrid", "Manchester", "Milan"
  , "Nice", "Oslo", "Palma de Mallorca", "Paris", "Prague", "Pristina"
  , "Rome", "Sarajevo", "Sofia", "Stockholm", "Tirana", "Venice", "Vienna", "Warsaw"
  ]

--------------------------------------------------------------------------------
-- Main Logic
--------------------------------------------------------------------------------

fetchFlights :: String -> IO ()
fetchFlights apiKey = runReq defaultHttpConfig $ do
  let url = https "serpapi.com" /: "search.json"
      options = 
          "engine" =: ("google_flights" :: Text)
       <> "departure_id" =: ("ZRH" :: Text)
       <> "arrival_id" =: ("BRU" :: Text)
       <> "outbound_date" =: ("2026-03-17" :: Text)
       <> "type" =: ("2" :: Text)
       <> "currency" =: ("CHF" :: Text)
       <> "hl" =: ("en" :: Text)
       <> "api_key" =: T.pack apiKey

  liftIO $ putStrLn "Fetching flight offers to Brussels (ZRH -> BRU) for tomorrow (2026-03-17) using SerpApi..."
  response <- req GET url NoReqBody jsonResponse options

  let offerResp = responseBody response :: SerpApiResponse
      best = case best_flights offerResp of
               Just fs -> fs
               Nothing -> []
      other = case other_flights offerResp of
                Just fs -> fs
                Nothing -> []
      allOffers = best ++ other

  liftIO $ do
    putStrLn "\n=== Cheapest Flights ==="
    if null allOffers
      then putStrLn "No flights found."
      else do
        let topOffers = take 3 allOffers
        mapM_ printOffer topOffers
    putStrLn "========================\n"

  where
    printOffer :: BestFlight -> IO ()
    printOffer o = do
      let flightNames = case flights o of
            Just fs -> T.intercalate ", " $ map (\f -> maybe "Unknown" id (airline f) <> " (" <> maybe "Unknown" id (flight_number f) <> ")") fs
            Nothing -> "Unknown"
      putStrLn $ "Airline: " ++ T.unpack flightNames ++ " | Price: " ++ show (maybe 0 id (price o)) ++ " CHF"

data RowData = RowData
  { rdDest :: String
  , rdPrice :: Int
  , rdStart :: String
  , rdEnd :: String
  , rdDur :: Int
  } deriving (Show)

fetchSwissFlightsTable :: String -> IO ()
fetchSwissFlightsTable apiKey = do
  now <- getCurrentTime
  let days = 24 * 60 * 60
  let todayStr = formatTime defaultTimeLocale "%Y-%m-%d" now
  let nextWeekStr = formatTime defaultTimeLocale "%Y-%m-%d" (addUTCTime (7 * days) now)
  
  putStrLn $ "Finding best Swiss direct return flights from Zurich..."
  putStrLn $ "Outbound: " ++ todayStr ++ " | Return: " ++ nextWeekStr
  putStrLn "Querying European destinations (this may take a moment)..."

  results <- runReq defaultHttpConfig $ do
    let url = https "serpapi.com" /: "search.json"
    
    forM swissEuropeanDestinationsFromZurich $ \dest -> do
      liftIO $ putStrLn $ " - Querying " ++ dest ++ "..."
      let options = "engine" =: ("google_flights" :: Text)
                 <> "departure_id" =: ("ZRH" :: Text)
                 <> "arrival_id" =: (T.pack dest)
                 <> "outbound_date" =: (T.pack todayStr)
                 <> "return_date" =: (T.pack nextWeekStr)
                 <> "type" =: ("1" :: Text) -- 1 = Round trip
                 <> "stops" =: ("1" :: Text) -- 1 = nonstop
                 <> "currency" =: ("CHF" :: Text)
                 <> "hl" =: ("en" :: Text)
                 <> "api_key" =: T.pack apiKey
      
      -- Avoid crashing loop if one fails by trying to catch exceptions or just failing
      resp <- req GET url NoReqBody jsonResponse options
      let respData = responseBody resp :: SerpApiResponse
      
      let allFs = maybe [] id (best_flights respData) ++ maybe [] id (other_flights respData)
          -- Filter for flights where all legs have "Swiss" in the airline
          swissOnly = filter isSwiss allFs
      
      if null swissOnly
        then return Nothing
        else do
          let best = head (sortBy (comparing price) swissOnly) -- Sort remaining by price Ascending
          return $ Just (dest, best)

  let rows = mapMaybe toRowData (catMaybes results)
      sortedRows = sortBy (comparing rdPrice) rows

  putStrLn "\n========================================================================================"
  printf "%-20s | %-16s | %-16s | %-10s | %-10s\n" ("Destination"::String) ("Start Time"::String) ("End Time"::String) ("Price CHF"::String) ("Duration m"::String)
  putStrLn "----------------------------------------------------------------------------------------"
  forM_ sortedRows $ \r -> do
      printf "%-20s | %-16s | %-16s | %-10d | %-10d\n" (rdDest r) (rdStart r) (rdEnd r) (rdPrice r) (rdDur r)
  putStrLn "========================================================================================\n"

  where
    isSwiss :: BestFlight -> Bool
    isSwiss bf = case flights bf of
      Just fs -> not (null fs) && all (\f -> maybe False (\a -> "Swiss" `T.isInfixOf` a) (airline f)) fs
      Nothing -> False

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
      putStrLn "Please get a free API key from https://serpapi.com/ and set it like this:"
      putStrLn "export SERPAPI_API_KEY=\"your_key_here\""
      putStrLn "Then run the client again."
      exitFailure
    Just key -> do
      fetchSwissFlightsTable key
