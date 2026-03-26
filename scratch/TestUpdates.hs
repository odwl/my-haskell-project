{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Scratch where

data City = City {name :: String} deriving Show
data Address = Address {city :: City, street :: String} deriving Show
data User = User {name :: String, address :: Address} deriving Show

updateCityName :: User -> String -> User
updateCityName u cName = u {address = u.address {city = u.address.city {name = cName}}}

main = print $ updateCityName (User "Alice" (Address (City "Tokyo") "Shibuya")) "London"
