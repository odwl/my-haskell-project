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

--------------------------------------------------------------------------------
-- SerpApi Response Data Types
--------------------------------------------------------------------------------

data FlightDetail = FlightDetail
  { airline :: Text
  , flight_number :: Text
  } deriving (Show, Generic)

instance FromJSON FlightDetail

data BestFlight = BestFlight
  { flights :: [FlightDetail]
  , price :: Int
  } deriving (Show, Generic)

instance FromJSON BestFlight

data SerpApiResponse = SerpApiResponse
  { best_flights :: Maybe [BestFlight]
  , other_flights :: Maybe [BestFlight]
  } deriving (Show, Generic)

instance FromJSON SerpApiResponse

--------------------------------------------------------------------------------
-- Main Logic
--------------------------------------------------------------------------------

fetchFlights :: String -> IO ()
fetchFlights apiKey = runReq defaultHttpConfig $ do
  -- URL: https://serpapi.com/search.json
  let url = https "serpapi.com" /: "search.json"
      
  let options = 
          "engine" =: ("google_flights" :: Text)
       <> "departure_id" =: ("ZRH" :: Text)
       <> "arrival_id" =: ("BRU" :: Text)
       <> "outbound_date" =: ("2026-03-17" :: Text)
       <> "currency" =: ("CHF" :: Text)
       <> "hl" =: ("en" :: Text)
       <> "api_key" =: (T.pack apiKey)

  liftIO $ putStrLn "Fetching flight offers to Brussels (ZRH -> BRU) for tomorrow (2026-03-17) using SerpApi..."

  -- Send GET Request
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
        -- Grab the top 3 cheapest flights found
        let topOffers = take 3 allOffers
        mapM_ printOffer topOffers
    putStrLn "========================\n"

  where
    printOffer :: BestFlight -> IO ()
    printOffer o = do
      let flightNames = T.intercalate ", " $ map (\f -> airline f <> " (" <> flight_number f <> ")") (flights o)
      putStrLn $ "Airline: " ++ T.unpack flightNames ++ " | Price: " ++ show (price o) ++ " CHF"

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
    Just key -> fetchFlights key
