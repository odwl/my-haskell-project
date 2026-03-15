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

data DailyWeather = DailyWeather
  { sunshine_duration :: Maybe [Maybe Double]
  } deriving (Show, Generic)

instance FromJSON DailyWeather

data WeatherResponse = WeatherResponse
  { current_weather :: CurrentWeather
  , daily :: Maybe DailyWeather
  }
  deriving (Show, Generic)

instance FromJSON WeatherResponse

data CityWeather = CityWeather
  { cityName :: Text
  , cityTemp :: Double
  , cityWind :: Double
  , sunshineToday :: Double
  , sunshineTomorrow :: Double
  , sunshineAfterTomorrow :: Double
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
              <> "daily" =: ("sunshine_duration" :: Text)
              <> "timezone" =: ("auto" :: Text)
      
      wRes <- req GET wUrl NoReqBody jsonResponse wOpts
      let wData = responseBody wRes :: WeatherResponse
      let current = current_weather wData
      
      -- Extract sunshine duration for the next three days
      let getSunHrs :: Int -> Double
          getSunHrs dayOffset = 
            let secs = case daily wData >>= sunshine_duration of
                         Just xs -> if length xs > dayOffset 
                                      then case xs !! dayOffset of
                                             Just s -> s
                                             Nothing -> 0.0
                                      else 0.0
                         Nothing -> 0.0
            in (fromIntegral (round (secs / 3600.0 * 10.0) :: Int) :: Double) / 10.0

      return $ Just CityWeather
        { cityName = name (loc :: GeocodeResult)
        , cityTemp = temperature current
        , cityWind = windspeed current
        , sunshineToday         = getSunHrs 0
        , sunshineTomorrow      = getSunHrs 1
        , sunshineAfterTomorrow = getSunHrs 2
        }
    _ -> return Nothing

-- | Helper to print the ranking for a specific day
printRanking :: Text -> (CityWeather -> Double) -> [CityWeather] -> IO ()
printRanking dayLabel getSunshine validResults = do
  let ranked = sortBy (flip (comparing getSunshine)) validResults
  TIO.putStrLn $ "\n=== Weather Ranked by Sunshine Duration (" <> dayLabel <> ", Decreasing) ==="
  forM_ ranked $ \cw -> do
    TIO.putStrLn $ cityName cw <> " -> " 
      <> T.pack (show (getSunshine cw)) <> " hrs (Temp: " 
      <> T.pack (show (cityTemp cw)) <> " °C, Wind: "
      <> T.pack (show (cityWind cw)) <> " km/h)"

main :: IO ()
main = runReq defaultHttpConfig $ do
  let cities = [ "Zurich", "Prague", "St. Moritz", "Davos", "Brussels"
               , "Arosa", "Lenzerheide", "Laax", "Corvatsch", "Obersaxen", "Andermatt"
               , "Maloja"
               ]
  
  liftIO $ putStrLn "Fetching weather for multiple cities..."
  
  resultsMb <- forM cities fetchCityWeather
  
  -- filter out un-found cities
  let validResults = [ cw | Just cw <- resultsMb ]
  
  liftIO $ do
    printRanking "Today" sunshineToday validResults
    printRanking "Tomorrow" sunshineTomorrow validResults
    printRanking "Day After Tomorrow" sunshineAfterTomorrow validResults

