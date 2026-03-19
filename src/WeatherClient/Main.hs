{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.List (sortBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics
import Network.HTTP.Req

data GeocodeResult = GeocodeResult
  { name :: Text,
    latitude :: Double,
    longitude :: Double
  }
  deriving (Show, Generic)

instance FromJSON GeocodeResult

newtype GeocodeResponse = GeocodeResponse
  { results :: Maybe [GeocodeResult]
  }
  deriving (Show, Generic)

instance FromJSON GeocodeResponse

data CurrentWeather = CurrentWeather
  { temperature_2m :: Double,
    wind_speed_10m :: Double,
    snow_depth :: Maybe Double
  }
  deriving (Show, Generic)

instance FromJSON CurrentWeather

data DailyWeather = DailyWeather
  { sunshine_duration :: Maybe [Maybe Double],
    snowfall_sum :: Maybe [Maybe Double]
  }
  deriving (Show, Generic)

instance FromJSON DailyWeather

data WeatherResponse = WeatherResponse
  { current :: CurrentWeather,
    daily :: Maybe DailyWeather
  }
  deriving (Show, Generic)

instance FromJSON WeatherResponse

data CityWeather = CityWeather
  { cityName :: Text,
    cityTemp :: Double,
    cityWind :: Double,
    sunshineToday :: Double,
    sunshineTomorrow :: Double,
    sunshineAfterTomorrow :: Double,
    snowToday :: Double,
    snowTomorrow :: Double,
    snowAfterTomorrow :: Double,
    currentSnowDepth :: Double
  }
  deriving (Show)

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
    Just (loc : _) -> do
      -- 2. Weather
      let wUrl = https "api.open-meteo.com" /: "v1" /: "forecast"
      let wOpts =
            "latitude" =: (latitude (loc :: GeocodeResult) :: Double)
              <> "longitude" =: (longitude (loc :: GeocodeResult) :: Double)
              <> "current" =: ("temperature_2m,wind_speed_10m,snow_depth" :: Text)
              <> "daily" =: ("sunshine_duration,snowfall_sum" :: Text)
              <> "timezone" =: ("auto" :: Text)

      wRes <- req GET wUrl NoReqBody jsonResponse wOpts
      let wData = responseBody wRes :: WeatherResponse
      let curr = current wData

      -- Extract sunshine duration for the next three days
      let getSunHrs :: Int -> Double
          getSunHrs dayOffset =
            let secs = case daily wData >>= sunshine_duration of
                  Just xs ->
                    if length xs > dayOffset
                      then fromMaybe 0.0 (xs !! dayOffset)
                      else 0.0
                  Nothing -> 0.0
             in (fromIntegral (round (secs / 3600.0 * 10.0) :: Int) :: Double) / 10.0

      let getSnowCm :: Int -> Double
          getSnowCm dayOffset =
            case daily wData >>= snowfall_sum of
              Just xs ->
                if length xs > dayOffset
                  then fromMaybe 0.0 (xs !! dayOffset)
                  else 0.0
              Nothing -> 0.0

      let baseSnowDepthCm = fromMaybe 0.0 (snow_depth curr) * 100.0

      return $
        Just
          CityWeather
            { cityName = name (loc :: GeocodeResult),
              cityTemp = temperature_2m curr,
              cityWind = wind_speed_10m curr,
              sunshineToday = getSunHrs 0,
              sunshineTomorrow = getSunHrs 1,
              sunshineAfterTomorrow = getSunHrs 2,
              snowToday = getSnowCm 0,
              snowTomorrow = getSnowCm 1,
              snowAfterTomorrow = getSnowCm 2,
              currentSnowDepth = baseSnowDepthCm
            }
    _ -> return Nothing

-- | Helper to print the ranking for current snow depth
printSnowRanking :: [CityWeather] -> IO ()
printSnowRanking validResults = do
  let ranked = sortBy (comparing (Down . currentSnowDepth)) validResults
  TIO.putStrLn "\n=== Weather Ranked by Current Snow Base Depth (Decreasing) ==="
  forM_ ranked $ \cw -> do
    TIO.putStrLn $
      cityName cw
        <> " -> "
        <> T.pack (show (round (currentSnowDepth cw) :: Int))
        <> " cm base ("
        <> T.pack (show (round (snowToday cw) :: Int))
        <> " cm fresh today, Temp: "
        <> T.pack (show (cityTemp cw))
        <> " °C, Wind: "
        <> T.pack (show (cityWind cw))
        <> " km/h)"

main :: IO ()
main = runReq defaultHttpConfig $ do
  let cities =
        [ 
          "St. Moritz",
          "Davos",
          "Arosa",
          "Lenzerheide",
          "Laax",
          "Corvatsch",
          "Obersaxen",
          "Andermatt",
          "Maloja"
        ]

  liftIO $ putStrLn "Fetching weather for multiple cities..."

  resultsMb <- forM cities fetchCityWeather

  -- filter out un-found cities
  let validResults = catMaybes resultsMb

  liftIO $ do
    printSnowRanking validResults
