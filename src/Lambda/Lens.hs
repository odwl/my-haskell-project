{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Lambda.Lens where

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

-----------------------------------
-- ADT
-- file:///Users/odwl/Downloads/main.pdf
-----------------------------------

data Metadata = Metadata { _fileName :: String, _owner :: String } deriving (Show, Eq)

data DocType = Text | Binary deriving (Show, Eq) 

data Document = Doc { _docType :: DocType, _metadata :: Metadata, _content :: String } deriving (Show, Eq)

data FileSystem = File Document 
                | Folder String [FileSystem]
                deriving (Show, Eq)

makeLenses ''Metadata
makeLenses ''Document
makePrisms ''FileSystem

-- A manual, recursive Traversal that focuses on every Document in a FileSystem tree.
allDocuments :: Traversal' FileSystem Document -- is a function (Document -> g Document) -> FileSystem -> g FileSystem  (where g is applicative)
allDocuments f (File d) = File <$> f d
-- Here we use `traversed` to iterate over the List of children,
-- and recursively apply `allDocuments` to each child!
allDocuments f (Folder folderName contents) = Folder folderName <$> traversed (allDocuments f) contents

searchFile :: FileSystem -> String -> [Document]
searchFile fs targetName = 
    toListOf (allDocuments . filtered (\doc -> doc ^. metadata . fileName == targetName)) fs

documentFlatList :: FileSystem -> [Document]
documentFlatList fs = toListOf allDocuments fs -- fs ^.. allDocuments

-- Idiomatic lens: Focus all the way to the string, then just check strings!
documentExist :: FileSystem -> String -> Bool
documentExist fs targetName = elemOf (allDocuments . metadata . fileName) targetName fs

-- Here are the 3 main ways to write that point-free predicate:
-- 1. Standard Composition:  ((== targetName) . view (metadata . fileName))
-- 2. Lens 'has' + 'only':   has (metadata . fileName . only targetName)
-- 3. The best lens way:     Focus all the way down and use (== targetName) directly!

-- -- | A simple file system data type.
-- data FileSystem = File { _name :: String, _size :: Int }
--                 | Folder { _name :: String, _contents :: [FileSystem] }
--                 deriving (Show)

-- -- Generate lenses for the FileSystem data type.
-- makeLenses ''FileSystem

