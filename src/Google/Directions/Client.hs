{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Google.Directions.Client (
        getDirections,
        ApiKey (ApiKey),
        Directions (..),
        Route (..),
        TextValue (..),
        Coord,
        Leg (..),
        Step (..),
        PolyLine (..),
        StatusCode (..),
    ) where

import           Data.Aeson              as JSON
import           Data.Aeson.Casing       (snakeCase, aesonDrop)
import           Data.ByteString         (fromStrict)
import qualified Data.Text               as T
import           GHC.Generics
import           Google.Directions.Query (DirectionsQuery, toQueryParams)
import           Network.Curl.Download
import           Network.URL             (exportURL)
import qualified Network.URL             as URL

data Directions = Directions {
    status :: StatusCode,
    routes :: [Route]
} deriving (Eq, Generic, Show)

instance JSON.FromJSON Directions where

data Route = Route {
    summary       :: T.Text,
    legs          :: [Leg],
    waypointOrder :: [Integer],
    --overviewPolyline :: PolyLine,
    copyrights    :: T.Text,
    warnings      :: [T.Text]
} deriving (Eq, Generic, Show)

instance JSON.FromJSON Route where
    parseJSON = JSON.genericParseJSON $ aesonDrop 0 snakeCase

data TextValue = TextValue { text :: T.Text, value :: Double } deriving (Eq, Generic, Show)
instance JSON.FromJSON TextValue where


data Coord = Coord { lng :: Double, lat :: Double } deriving (Eq,Generic, Show)
instance JSON.FromJSON Coord where

data Leg = Leg {
    legSteps         :: [Step],
    legDistance      :: TextValue,
    legDuration      :: TextValue,
    legStartLocation :: Coord,
    legEndLocation   :: Coord,
    legStartAddress  :: T.Text,
    legEndAddress    :: T.Text
} deriving (Eq, Generic, Show)

instance JSON.FromJSON Leg where
    parseJSON = JSON.genericParseJSON $ aesonDrop 3 snakeCase

data Step = Step {
    stepHtmlInstructions  :: T.Text,
    stepDistance      :: TextValue,
    stepDuration      :: TextValue,
    stepStartLocation :: Coord,
    stepEndLocation   :: Coord
} deriving (Eq, Generic, Show)

instance JSON.FromJSON Step where
    parseJSON = JSON.genericParseJSON $ aesonDrop 4 snakeCase

data PolyLine = PolyLine {
    points :: T.Text,
    levels :: T.Text
} deriving (Eq, Generic, Show)

data StatusCode = OK
    | NotFound
    | ZeroResults
    | MaxWaypointsExceeded
    | InvalidRequest
    | OverQueryLimit
    | RequestDenied
    | UnknownError
        deriving (Eq, Show)

instance JSON.FromJSON StatusCode where
    parseJSON (JSON.String "OK") = return OK
    parseJSON (JSON.String "NOT_FOUND") = return NotFound
    parseJSON (JSON.String "ZERO_RESULTS") = return ZeroResults
    parseJSON (JSON.String "MAX_WAYPOINTS_EXCEEDED") = return MaxWaypointsExceeded
    parseJSON (JSON.String "INVALID_REQUEST") = return InvalidRequest
    parseJSON (JSON.String "OVER_QUERY_LIMIT") = return OverQueryLimit
    parseJSON (JSON.String "REQUEST_DENIED") = return RequestDenied
    parseJSON (JSON.String v) = fail $ "Unknown status code: " ++ T.unpack v
    parseJSON _ = fail "Expected json string"

newtype ApiKey = ApiKey { getApiKey :: T.Text }

getDirections :: ApiKey -> DirectionsQuery -> IO (Either String Directions)
getDirections apiKey query = do
    let
        host = URL.Host (URL.HTTP True) "maps.googleapis.com" Nothing
        absoluteUrl = URL.Absolute host
        params = toQueryParams query <> [("key", T.unpack $ getApiKey apiKey)]
        url = URL.URL absoluteUrl "/maps/api/directions/json" params

    rawResponseStr <- openURI $ exportURL url
    return $ rawResponseStr >>= JSON.eitherDecode . fromStrict

