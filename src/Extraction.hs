module Extraction where

import DbStore
import Poi
import Geo.Computations
import Geo.Types
import Data.List

data CalculatedProperty = CalculatedProperty
                    { property :: Property
                    , distToColes :: Integer
                    , distToWoolworth :: Integer
                    , distToAldi :: Integer
                    , distToTrain :: Integer
                    , distToTram :: Integer
                    , distToBus :: Integer
                    } deriving (Show)


fromProperty :: [Stop] -> [Store] -> Property -> CalculatedProperty
fromProperty stops stores p = CalculatedProperty
    { property = p
    , distToColes = dtColes
    , distToWoolworth = dtWoolworth
    , distToAldi = dtAldi
    , distToTrain = dtTrain
    , distToTram = dtTram
    , distToBus = dtBus
    }
    where
        distToStore = calculateDistToStore (toPoint (lat p) (lng p)) stores
        distToStop = calculateDistToStop (toPoint (lat p) (lng p)) stops
        dtColes = distToStore Coles
        dtWoolworth = distToStore Woolworths
        dtAldi = distToStore Aldi
        dtTrain = distToStop Train
        dtTram = distToStop Tram
        dtBus = distToStop Bus

calculateDistToStore :: Point -> [Store] -> StoreType -> Integer
calculateDistToStore propertyLocation stores sType = round closest
    where
        filteredStores = filter (\s -> (storeType s) == sType) stores
        distances = fmap (\s ->
            distance (toPoint (storeLatitude s) (storeLongitude s)) propertyLocation
            ) filteredStores
        closest = head $ sort distances

calculateDistToStop :: Point -> [Stop] -> StopType -> Integer
calculateDistToStop propertyLocation stops sType = round closest
    where
        filteredStops = filter (\s -> (stopType s) == sType) stops
        distances = fmap (\s ->
            distance (toPoint (stopLatitude s) (stopLongitude s)) propertyLocation
            ) filteredStops
        closest = head $ sort distances

toPoint :: Double -> Double -> Point
toPoint latitude longitude =
    pt latitude longitude Nothing Nothing

toDisplayLine :: CalculatedProperty -> String
toDisplayLine cp = (numberToString $ distToColes cp) ++
    (numberToString $ distToWoolworth cp) ++
    (numberToString $ distToAldi cp) ++
    (numberToString $ distToTrain cp) ++
    (numberToString $ distToTram cp) ++
    (numberToString $ distToBus cp) ++
    " " ++ (link (property cp)) ++
    " " ++ (show $ lat (property cp)) ++
    " " ++ (show $ lng (property cp))

numberToString :: Integer -> String
numberToString number = s ++ (replicate (15 - length s) ' ')
    where
        s = show number
