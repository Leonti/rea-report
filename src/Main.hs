module Main where

import DbStore
import Database.SQLite.Simple
import Data.List
import Data.Ord
import Poi
import Extraction

main :: IO ()
main = do
    allProperties <- readAllProperties
    let uniqueProperties = groupBy isTheSameProperty (sortBy (comparing link) allProperties)
    stops <- readStops
    stores <- readStores
    let calculatedProperties = fmap ((fromProperty stops stores) . last) uniqueProperties
    let toPrint = fmap toDisplayLine calculatedProperties
    _ <- mapM_ putStrLn toPrint

    putStrLn "hello world"

isTheSameProperty :: Property -> Property -> Bool
isTheSameProperty p1 p2 = link p1 == link p2

readStops :: IO [Stop]
readStops = do
    contents <- readFile "stops.csv"
    return $ map lineToStop $ lines contents

readStores :: IO [Store]
readStores = do
    contents <- readFile "stores.csv"
    return $ map lineToStore $ lines contents

readAllProperties :: IO [Property]
readAllProperties = do
    conn <- open "properties.db"
    allPropertyRows <- query_ conn propertyRowSelectAllQuery :: IO [PropertyRow]
    Database.SQLite.Simple.close conn
    return (fmap toProperty allPropertyRows)
