{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Lambda.UserAddressCity where

import Control.Lens

-----------------------------------
-- Lenses
-- file:///Users/odwl/Downloads/main.pdf
-----------------------------------

-- | A simple city data type. See what happens.
newtype City = City {_cityName :: String} deriving (Show, Eq)

makeFields ''City

-- | A simple address data type.
data Address = Address {_addressCity :: City, _addressStreet :: String} deriving (Show, Eq)

makeFields ''Address

-- | A simple user data type.
data User = User {_userName :: String, _userAddress :: Address} deriving (Show, Eq)

makeFields ''User

-- | Example usage of nested field access.
exampleAccess :: User -> City
-- exampleAccess = view (address . city)
exampleAccess u = u ^. (address . city)


-- | Example usage to get the city name.
exampleAccessName :: User -> String
exampleAccessName = view (address . city . name)

-- | Lenses for our data types
-- | Update the city of a user's address.
updateCity :: User -> City -> User
updateCity u c = set (address . city) c u

-- | Update the city name of a user's address.
updateCityName :: User -> String -> User
updateCityName u cName = set (address . city . name) cName u
