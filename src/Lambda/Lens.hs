{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

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

-- The recursive, polymorphic tree data structure
data File a = File a 
            | Folder String [File a]
            deriving (Show, Eq)

-- The type synonym to make the code cleaner
type FileSystem = File Document


makeLenses ''Metadata
makeLenses ''Document
makePrisms ''File

instance Foldable File where
    foldMap f (File doc) = f doc
    foldMap f (Folder _ cs) = foldMap (foldMap f) cs

flattenFolders :: File Document -> [Document]
flattenFolders doc = foldMap (:[]) doc

flattenFolders2 :: File Document -> [Document]
flattenFolders2 doc = doc ^.. folded

searchFiles' :: String -> File Document -> [Document]
searchFiles' targetName doc = filter ((== targetName) . view (metadata . fileName)) (flattenFolders doc)

instance Plated FileSystem where
    plate = _Folder . _2 . traversed

-- A Plated fold that focuses on every Document in a FileSystem tree.
documentFold :: Fold FileSystem Document 
documentFold = cosmos . _File

documentFlatList :: FileSystem -> [Document]
documentFlatList fs = fs ^.. documentFold

fileNameFold :: Fold FileSystem String
fileNameFold = documentFold . metadata . fileName

searchFile :: FileSystem -> String -> [Document]
searchFile fs targetName = 
    toListOf (documentFold . filtered ((== targetName) . view (metadata . fileName))) fs

-- Idiomatic lens: Focus all the way to the string, then just check strings!
documentExist :: FileSystem -> String -> Bool
documentExist fs targetName = elemOf fileNameFold targetName fs