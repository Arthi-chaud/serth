{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Main (main) where

import Control.Concurrent
import Control.DeepSeq
import Control.Monad
import Criterion.Main
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Functor
import Data.Serth.Serialiser.Format.JSON
import qualified Data.Serth.Serialiser.Format.JSON as F
import qualified Data.Serth.Serialiser.Serialisable.TH as F
import Futadata ()
import GHC.Generics
import Network.HTTP (getResponseBody, mkRequest, simpleHTTP)
import Network.HTTP.Base (Request (..), RequestMethod (..))
import Network.HTTP.Headers (Header (..), HeaderName (..))
import Network.URI (parseURI)
import Network.Wai.Handler.Warp
import Servant hiding (GET, Header, JSON)
import qualified Servant.API as S
import Text.Printf (printf)

data Forecast = Forecast
    { city :: City
    , time :: String
    , temperature :: Temperature
    , condition :: Condition
    }
    deriving (Eq, Show, Generic)

instance Aeson.ToJSON Forecast
data Condition
    = Rain {probability :: Float}
    | Sunny
    | Cloudy
    | Thunder
    deriving (Eq, Show, Generic)

instance Aeson.ToJSON Condition

data Temperature = Temperature {c :: Float, f :: Float} deriving (Eq, Show, Generic)

instance Aeson.ToJSON Temperature
data Coord = Coord {latitude :: Float, longitude :: Float} deriving (Eq, Show, Generic)

instance Aeson.ToJSON Coord
data Country = Country {countryName :: String, countryCoord :: Coord} deriving (Eq, Show, Generic)
instance Aeson.ToJSON Country

data City = City {cityName :: String, country :: Country, cityCoord :: Coord} deriving (Eq, Show, Generic)

instance Aeson.ToJSON City

$( F.genSerialisables @F.JSON
    [ ''Condition
    , ''Temperature
    , ''Coord
    , ''Country
    , ''City
    , ''Forecast
    ]
 )

type API =
    "aeson" :> Capture "n" Int :> Get '[S.JSON] [Forecast]
        :<|> "futa" :> Capture "n" Int :> Get '[F.JSON] [Forecast]

item :: Forecast
item =
    Forecast
        ( City
            "Paris"
            (Country "France" (Coord 3.14 10000))
            (Coord (2 ^ (4 :: Int)) (fromIntegral (maxBound :: Int)))
        )
        "Wed Jul 30 2025 10:30:40 GMT+0100 (British Summer Time)"
        (Temperature 10 56)
        (Rain 0.6)

endpoint :: Int -> Handler [Forecast]
endpoint = return . flip replicate item

server_ :: Server API
server_ = endpoint :<|> endpoint

serverAPI :: Proxy API
serverAPI = Proxy

app :: Application
app = serve serverAPI server_

main :: IO ()
main = do
    thread <- forkIO $ run 8081 app
    defaultMain [bgroup "servant" $ bench_ <$> listLength]
    killThread thread
  where
    listLength = [1, 5, 10, 20, 50, 100, 150, 200]

bench_ :: Int -> Benchmark
bench_ depth =
    bgroup (show depth) $
        ["aeson", "futa" :: String]
            <&> (\name -> env (return $ buildReq name depth) $ bench name . whnfAppIO (simpleHTTP >=> getResponseBody))
  where
    buildReq :: String -> Int -> Request ByteString
    buildReq name length_ = case parseURI (printf "http://localhost:8081/%s/%d" name length_) of
        Just uri -> mkRequest GET uri
        _ -> error "Parsing URL failed"

deriving instance Generic (Request a)
deriving instance Generic RequestMethod
deriving instance Generic Header
deriving instance Generic HeaderName
instance (NFData a) => NFData (Request a)
instance NFData RequestMethod
instance NFData Header
instance NFData HeaderName
