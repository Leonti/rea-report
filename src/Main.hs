module Main where

import DbStore
import Database.SQLite.Simple
import Data.List

main :: IO ()
main = do
    allProperties <- readAllProperties
    let uniquePropertyLinks = nub $ fmap address allProperties
    _ <- print $ "Count: " ++ show (length uniquePropertyLinks)

    putStrLn "hello world"

readAllProperties :: IO [Property]
readAllProperties = do
    conn <- open "properties.db"
    allPropertyRows <- query_ conn propertyRowSelectAllQuery :: IO [PropertyRow]
    Database.SQLite.Simple.close conn
    return (fmap toProperty allPropertyRows)
