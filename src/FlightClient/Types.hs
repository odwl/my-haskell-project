{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Req

--------------------------------------------------------------------------------
-- SerpApi Response Data Types
--------------------------------------------------------------------------------

newtype AirportInfo = AirportInfo
  { time :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON AirportInfo

data FlightDetail = FlightDetail
  { airline :: Maybe Text,
    flight_number :: Maybe Text,
    departure_airport :: Maybe AirportInfo,
    arrival_airport :: Maybe AirportInfo,
    duration :: Maybe Int
  }
  deriving (Show, Generic)

instance FromJSON FlightDetail

data BestFlight = BestFlight
  { flights :: Maybe [FlightDetail],
    price :: Maybe Int
  }
  deriving (Show, Generic)

instance FromJSON BestFlight

data SerpApiResponse = SerpApiResponse
  { best_flights :: Maybe [BestFlight],
    other_flights :: Maybe [BestFlight]
  }
  deriving (Show, Generic)

instance FromJSON SerpApiResponse

--------------------------------------------------------------------------------
-- API Call types
--------------------------------------------------------------------------------

-- | Parameters for searching flights
data FlightSearchOptions = FlightSearchOptions
  { departureId :: Text,
    arrivalId :: Text,
    outboundDate :: Text,
    returnDate :: Maybe Text,
    travelType :: Text, -- "1" for round trip, "2" for one way
    stops :: Maybe Text, -- "1" for nonstop
    currency :: Text,
    apiKey :: Text
  }

-- | Capability to search for flights (Dependency Injection)
class (Monad m) => MonadFlightSearch m where
  searchFlightsM :: FlightSearchOptions -> m SerpApiResponse

--------------------------------------------------------------------------------
-- Internal UI Logic Types
--------------------------------------------------------------------------------

data RowData = RowData
  { rdDest :: String,
    rdAirline :: String,
    rdPrice :: Int,
    rdStart :: String,
    rdEnd :: String,
    rdDur :: Int
  }
  deriving (Show)

-- | Raw SerpApi flight search
searchFlights :: FlightSearchOptions -> Req SerpApiResponse
searchFlights opts = do
  let url = https "serpapi.com" /: "search.json"
      params =
        "engine" =: ("google_flights" :: Text)
          <> "departure_id" =: departureId opts
          <> "arrival_id" =: arrivalId opts
          <> "outbound_date" =: outboundDate opts
          <> "type" =: travelType opts
          <> "currency" =: currency opts
          <> "hl" =: ("en" :: Text)
          <> "api_key" =: apiKey opts

      -- Optional params
      finalParams =
        params
          <> maybe mempty ("return_date" =:) (returnDate opts)
          <> maybe mempty ("stops" =:) (stops opts)

  response <- req GET url NoReqBody jsonResponse finalParams
  return (responseBody response)

instance MonadFlightSearch IO where
  searchFlightsM opts = runReq defaultHttpConfig (searchFlights opts)
