{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Main (main) where

import Control.Concurrent
import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import Criterion.Main
import Data
import Data.ByteString.Lazy (ByteString, fromStrict, toStrict)
import Data.Functor
import qualified Data.Serth.Serialiser.Format.JSON as Serth
import Data.Serth.Serialiser.Serialisable
import GHC.Generics
import GHC.IO (unsafePerformIO)
import GHC.IORef
import MB (loadDataSet)
import Network.HTTP (getResponseBody, mkRequest, simpleHTTP)
import Network.HTTP.Base (Request (..), RequestMethod (..))
import Network.HTTP.Headers (Header (..), HeaderName (..))
import Network.URI (parseURI)
import Network.Wai.Handler.Warp
import Servant hiding (GET, Header, JSON)
import qualified Servant.API as S
import Text.Printf (printf)

{-# NOINLINE db #-}
db :: IORef [Label]
db = unsafePerformIO $ do
    s <- loadDataSet "benchmark/servant/dataset.json"
    newIORef $! force (translateDataSet s)

instance Accept Serth.JSON where
    contentTypes _ = contentTypes (Proxy :: Proxy S.JSON)

instance (Serialisable Serth.JSON a) => MimeRender Serth.JSON a where
    mimeRender _ = fromStrict . (serialise @Serth.JSON)

type API =
    "aeson" :> Capture "n" Int :> Get '[S.JSON] Page
        :<|> "serth" :> Capture "n" Int :> Get '[Serth.JSON] Page

endpoint :: Int -> Handler Page
endpoint n = do
    items <- take n <$> liftIO (readIORef db)
    return $ Page "2000-01-01 00:00:00" n 0 items

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
    listLength = [10, 25, 50, 100]

bench_ :: Int -> Benchmark
bench_ depth =
    bgroup (show depth) $
        ["aeson", "serth" :: String]
            <&> (\n -> env (return $ buildReq n depth) $ bench n . nfAppIO (simpleHTTP >=> (fmap toStrict . getResponseBody)))
  where
    buildReq :: String -> Int -> Request ByteString
    buildReq n length_ = case parseURI (printf "http://localhost:8081/%s/%d" n length_) of
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
