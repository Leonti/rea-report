{-# LANGUAGE OverloadedStrings #-}

module DbStore where
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow

-- "CREATE TABLE properties (link TEXT, date TEXT, bedrooms INTEGER, bathrooms INTEGER, cars INTEGER, location TEXT, price INTEGER, lat REAL, lng REAL);"


data PropertyRow = PropertyRow String String Int Int Int String Int Double Double deriving (Show)

instance FromRow PropertyRow where
    fromRow = PropertyRow <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data Property = Property
                    { link :: String
                    , date :: String
                    , bedrooms :: Int
                    , bathrooms :: Int
                    , cars :: Int
                    , address :: String
                    , price :: Int
                    , lat :: Double
                    , lng :: Double
                    } deriving (Show)

toProperty :: PropertyRow -> Property
toProperty (PropertyRow link date bedrooms bathrooms cars address price lat lng) =
    Property
    { link = link
    , date = date
    , bedrooms = bedrooms
    , bathrooms = bathrooms
    , cars = cars
    , address = address
    , price = price
    , lat = lat
    , lng = lng
    }

propertyRowSelectAllQuery :: Query
propertyRowSelectAllQuery = "SELECT * from properties"

propertiesForDate :: Query
propertiesForDate = "SELECT * from properties where date=?"
