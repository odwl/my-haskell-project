{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Mock where

import Control.Monad.IO.Class (MonadIO)
import Types

-- | A simple mock monad that just runs in IO but returns fake data
-- In a more advanced version, this could be a State monad to simulate different scenarios.
newtype MockApp a = MockApp {runMockApp :: IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadFlightSearch MockApp where
  searchFlightsM opts =
    return $
      SerpApiResponse
        { best_flights = Just [fakeFlight (arrivalId opts)],
          other_flights = Nothing
        }
    where
      fakeFlight _iata =
        BestFlight
          { flights =
              Just
                [ FlightDetail
                    { airline = Just "FakeAir",
                      flight_number = Just "FA123",
                      departure_airport = Just (AirportInfo (Just "2026-03-23T10:00")),
                      arrival_airport = Just (AirportInfo (Just "2026-03-23T12:00")),
                      duration = Just 120
                    }
                ],
            price = Just 99
          }
