{-# LANGUAGE TupleSections #-}
module Google.Directions.Query (DirectionsQuery, createDirectionsQuery, toQueryParams, withArrivalTime, withDepartureTime, withTravelMode, withTransitMode, withTransitPreference, withTrafficModel, withWaypoints, withIncludeAlternateRoutes, withAvoidables, withUnits) where

import System.Posix.Types (EpochTime)
import qualified Data.Text as T
import qualified Data.List as L
import Data.Maybe (catMaybes)

type OriginAddress = String
type DestinationAddress = String
type ArrivalTime = EpochTime
type DepartureTime = EpochTime

data TrafficModel = BestGuess
    | Optimistic
    | Pessimistic
    deriving (Eq)

instance Show TrafficModel where
    show BestGuess = "best_guess"
    show Optimistic = "optimistic"
    show Pessimistic = "pessimistic"

data TransitMode = Bus 
    | Subway
    | Train
    | Tram 
    | Rail
    deriving (Eq)

instance Show TransitMode where
    show Bus = "bus"
    show Subway = "subway"
    show Train = "train"
    show Tram = "tram"
    show Rail = "rail"

data TransitPreference = LessWalking | FewerTransfers deriving (Eq)
instance Show TransitPreference where
    show LessWalking = "less_walking"
    show FewerTransfers = "fewer_transfers"

data TravelMode = Driving
    | Walking
    | Bicycling
    | Transit
    deriving (Eq)

instance Show TravelMode where
    show Driving   = "driving"
    show Walking   = "walking"
    show Bicycling = "bicycling"
    show Transit   = "transit"

type OptimizeWaypoints = Bool
data Waypoints = Waypoints OptimizeWaypoints [T.Text] deriving (Eq)
instance Show Waypoints where
    show (Waypoints _ []) = ""
    show (Waypoints optimize points) =
        T.unpack $ (if optimize then "optimize:true|" else "") <> showWaypoints points
            where
                showWaypoints :: [T.Text] -> T.Text
                showWaypoints []      = ""
                showWaypoints [point] = point
                showWaypoints (p:ps)  = p <> "|" <> showWaypoints ps

data Avoidable = Tolls
    | Highways
    deriving (Eq, Show)

data Units = Imperial
    | Metric
    deriving (Eq)

instance Show Units where
    show Imperial = "imperial"
    show Metric   = "metric"
data DirectionsQuery = DirectionsQuery
    {
        getOrigin :: OriginAddress,
        getDestination :: DestinationAddress,
        getArrivalTime :: Maybe ArrivalTime,
        getDepartureTime :: Maybe DepartureTime,
        getTravelMode :: Maybe TravelMode,
        getTransitMode :: Maybe [TransitMode],
        getTransitPreference :: Maybe TransitPreference,
        getTrafficModel :: Maybe TrafficModel,
        getWaypoints :: Maybe Waypoints,
        getIncludeAlternateRoutes :: Bool,
        getAvoidables :: [Avoidable],
        getUnits :: Maybe Units
    }

createDirectionsQuery :: OriginAddress -> DestinationAddress -> DirectionsQuery
createDirectionsQuery origin dest = DirectionsQuery
    {
        getOrigin = origin,
        getDestination = dest,
        getArrivalTime = Nothing,
        getDepartureTime = Nothing,
        getTravelMode = Nothing,
        getTransitMode = Nothing,
        getTransitPreference = Nothing,
        getTrafficModel = Nothing,
        getWaypoints = Nothing,
        getIncludeAlternateRoutes = False,
        getAvoidables = mempty,
        getUnits = Nothing
    }

withArrivalTime :: ArrivalTime -> DirectionsQuery -> DirectionsQuery
withArrivalTime arrival x = x { getArrivalTime = Just arrival }

withDepartureTime :: DepartureTime -> DirectionsQuery -> DirectionsQuery
withDepartureTime depart x = x { getDepartureTime = Just depart }

withTravelMode :: TravelMode -> DirectionsQuery -> DirectionsQuery
withTravelMode mode x = x { getTravelMode = Just mode }

withTransitMode :: [TransitMode] -> DirectionsQuery -> DirectionsQuery
withTransitMode mode x = x { getTransitMode = Just mode }

withTransitPreference :: TransitPreference -> DirectionsQuery -> DirectionsQuery
withTransitPreference pref x = x { getTransitPreference = Just pref }

withTrafficModel :: TrafficModel -> DirectionsQuery -> DirectionsQuery
withTrafficModel model x = x { getTrafficModel = Just model }

withWaypoints :: Waypoints -> DirectionsQuery -> DirectionsQuery
withWaypoints waypoints x = x { getWaypoints = Just waypoints }

withIncludeAlternateRoutes :: Bool -> DirectionsQuery -> DirectionsQuery
withIncludeAlternateRoutes include x = x { getIncludeAlternateRoutes = include }

withAvoidables :: [Avoidable] -> DirectionsQuery -> DirectionsQuery
withAvoidables avoidables x = x { getAvoidables = avoidables }

withUnits :: Units -> DirectionsQuery -> DirectionsQuery
withUnits u x = x { getUnits = Just u }

boolToParamValue :: Bool -> String
boolToParamValue True  = "true"
boolToParamValue False = "false"

showAvoidables :: [Avoidable] -> Maybe String
showAvoidables [] = Nothing
showAvoidables xs = Just $ L.intercalate "," $ map show xs

toListValue :: (Show a) => [a] -> Maybe String
toListValue [] = Nothing
toListValue xs = Just $ L.intercalate "," $ map show xs

toQueryParams :: DirectionsQuery -> [(String, String)]
toQueryParams directions =
    catMaybes [
        Just ("origin", getOrigin directions),
        Just ("destination", getDestination directions),
        (\x -> ("arrival_time", show x)) <$> getArrivalTime directions,
        (\x -> ("departure_time", show x)) <$> getDepartureTime directions,
        (\x -> ("mode", show x)) <$> getTravelMode directions,
        (\x -> ("waypoints", show x)) <$> getWaypoints directions,
        (\x -> ("traffic_model", show x)) <$> getTrafficModel directions,
        Just ("alternatives", boolToParamValue $ getIncludeAlternateRoutes directions),
        ("avoid", ) <$> showAvoidables (getAvoidables directions),
        (\x -> ("units", show x)) <$> getUnits directions,
        (\x -> ("transit_mode", show x)) <$> (getTransitMode directions >>= toListValue),
        (\x -> ("transit_routing_preference", show x)) <$> getTransitPreference directions
    ]

