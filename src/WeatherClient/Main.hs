{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import GHC.Generics
import Network.HTTP.Req
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (sortBy)
import Data.Ord (comparing)
import Control.Monad (forM, forM_)

data GeocodeResult = GeocodeResult
  { name :: Text
  , latitude :: Double
  , longitude :: Double
  } deriving (Show, Generic)

instance FromJSON GeocodeResult

data GeocodeResponse = GeocodeResponse
  { results :: Maybe [GeocodeResult]
  } deriving (Show, Generic)

instance FromJSON GeocodeResponse

data CurrentWeather = CurrentWeather
  { temperature :: Double
  , windspeed :: Double
  , winddirection :: Double
  , weathercode :: Int
  }
  deriving (Show, Generic)

instance FromJSON CurrentWeather

data WeatherResponse = WeatherResponse
  { current_weather :: CurrentWeather
  }
  deriving (Show, Generic)

instance FromJSON WeatherResponse

data CityWeather = CityWeather
  { cityName :: Text
  , cityTemp :: Double
  , cityWind :: Double
  } deriving (Show)

fetchCityWeather :: Text -> Req (Maybe CityWeather)
fetchCityWeather city = do
  -- 1. Geocoding
  let geoUrl = https "geocoding-api.open-meteo.com" /: "v1" /: "search"
  let geoOpts =
        "name" =: city
          <> "count" =: (1 :: Int)
          <> "language" =: ("en" :: Text)
          <> "format" =: ("json" :: Text)
  
  geoRes <- req GET geoUrl NoReqBody jsonResponse geoOpts
  let geoData = responseBody geoRes :: GeocodeResponse
  
  case results geoData of
    Just (loc:_) -> do
      -- 2. Weather
      let wUrl = https "api.open-meteo.com" /: "v1" /: "forecast"
      let wOpts =
            "latitude" =: (latitude (loc :: GeocodeResult) :: Double)
              <> "longitude" =: (longitude (loc :: GeocodeResult) :: Double)
              <> "current_weather" =: True
      
      wRes <- req GET wUrl NoReqBody jsonResponse wOpts
      let wData = responseBody wRes :: WeatherResponse
      let current = current_weather wData
      
      return $ Just CityWeather
        { cityName = name (loc :: GeocodeResult)
        , cityTemp = temperature current
        , cityWind = windspeed current
        }
    _ -> return Nothing

main :: IO ()
main = runReq defaultHttpConfig $ do
  let cities = ["Zurich", "Prague", "St. Moritz", "Davos", "Brussels"]
  
  liftIO $ putStrLn "Fetching weather for multiple cities..."
  
  resultsMb <- forM cities fetchCityWeather
  
  -- filter out un-found cities
  let validResults = [ cw | Just cw <- resultsMb ]
  
  -- Rank by temperature in decreasing order
  let ranked = sortBy (flip (comparing cityTemp)) validResults
  
  liftIO $ do
    putStrLn "\n=== Weather Ranked by Temperature (Decreasing) ==="
    forM_ ranked $ \cw -> do
      TIO.putStrLn $ cityName cw <> " -> " 
        <> T.pack (show (cityTemp cw)) <> " °C (Wind: " 
        <> T.pack (show (cityWind cw)) <> " km/h)"
