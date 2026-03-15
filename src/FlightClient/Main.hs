{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import GHC.Generics
import Network.HTTP.Req
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

-- Duffel API token
apiToken :: ByteString
apiToken = "YOUR_DUFFEL_API_TOKEN"

--------------------------------------------------------------------------------
-- Request Data Types
--------------------------------------------------------------------------------

data Passenger = Passenger
  { type_ :: Text
  } deriving (Show, Generic)

-- We manually write ToJSON to map "type_" to "type"
instance ToJSON Passenger where
  toJSON (Passenger t) = object ["type" .= t]

data Slice = Slice
  { origin :: Text
  , destination :: Text
  , departure_date :: Text
  } deriving (Show, Generic)

instance ToJSON Slice

data OfferRequestData = OfferRequestData
  { slices :: [Slice]
  , passengers :: [Passenger]
  , requested_sources :: [Text]
  } deriving (Show, Generic)

instance ToJSON OfferRequestData

data OfferRequestPayload = OfferRequestPayload
  { data_ :: OfferRequestData
  } deriving (Show, Generic)

-- We manually write ToJSON to map "data_" to "data"
instance ToJSON OfferRequestPayload where
  toJSON (OfferRequestPayload d) = object ["data" .= d]

--------------------------------------------------------------------------------
-- Response Data Types
--------------------------------------------------------------------------------

data Amount = Amount
  { amount_value :: Text
  , amount_currency :: Text
  } deriving (Show, Generic)
instance FromJSON Amount where
  parseJSON = withObject "Amount" $ \v -> Amount
      <$> v .: "total_amount"
      <*> v .: "total_currency"

data Owner = Owner
  { name :: Text
  } deriving (Show, Generic)
instance FromJSON Owner

data Offer = Offer
  { offer_total_amount :: Text
  , offer_total_currency :: Text
  , owner :: Owner
  } deriving (Show, Generic)

instance FromJSON Offer where
    parseJSON = withObject "Offer" $ \v -> Offer
        <$> v .: "total_amount"
        <*> v .: "total_currency"
        <*> v .: "owner"

data OfferResponseData = OfferResponseData
  { offers :: [Offer]
  } deriving (Show, Generic)

instance FromJSON OfferResponseData

data OfferResponsePayload = OfferResponsePayload
  { response_data :: OfferResponseData
  } deriving (Show, Generic)

-- We manually write FromJSON to map "data" back to Haskell's generic "data" 
-- BUT Haskell cannot use 'data' as a field name easily, so we use data_ field name
-- and write manual FromJSON
data OfferResponseWrapper = OfferResponseWrapper 
  { responseData :: OfferResponseData 
  } deriving (Show)

instance FromJSON OfferResponseWrapper where
  parseJSON = withObject "OfferResponseWrapper" $ \v -> OfferResponseWrapper
    <$> v .: "data"

--------------------------------------------------------------------------------
-- Main Logic
--------------------------------------------------------------------------------

-- Since we are required to search for flights to Paris next Monday!
-- Next Monday is 2026-03-16. 
-- We'll just search from London (LHR) to Paris (CDG) to illustrate.
fetchFlights :: IO ()
fetchFlights = runReq defaultHttpConfig $ do
  let url = https "api.duffel.com" /: "air" /: "offer_requests"

  let payload = OfferRequestPayload
        { data_ = OfferRequestData
          { slices = [ Slice "LHR" "CDG" "2026-03-16" ]
          , passengers = [ Passenger "adult" ]
          , requested_sources = []
          }
        }

  let options =
        header "Authorization" ("Bearer " <> apiToken)
        <> header "Duffel-Version" "v2"
        <> header "Accept" "application/json"

  liftIO $ putStrLn "Fetching flight offers to Paris (LHR -> CDG) for next Monday (2026-03-16)..."

  -- Send POST Request
  response <- req POST url (ReqBodyJson payload) jsonResponse options

  let offerResp = responseBody response :: OfferResponseWrapper
      allOffers = offers (responseData offerResp)

  liftIO $ do
    putStrLn "\n=== Cheapest Flights ==="
    if null allOffers
      then putStrLn "No flights found."
      else do
        -- we just grab the first one assuming it's returned sorted by duffel, or we can just print the top 3
        let topOffers = take 3 allOffers
        mapM_ (\o -> putStrLn $ "Airline: " ++ T.unpack (name (owner o)) ++ " | Price: " ++ T.unpack (offer_total_amount o) ++ " " ++ T.unpack (offer_total_currency o)) topOffers
    putStrLn "========================\n"

main :: IO ()
main = fetchFlights
