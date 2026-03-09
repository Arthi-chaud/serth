{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Concurrent
import Control.DeepSeq
import Control.Monad
import Criterion.Main
import Data.Aeson (encode)
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (ByteString, fromStrict, toStrict)
import qualified Data.ByteString.Lazy as BS
import Data.Functor ((<&>))
import qualified Data.JSON.FromJSON as R
import Data.Maybe (fromMaybe)
import Data.Serth.Serialiser
import Data.Serth.Serialiser.Format.JSON
import qualified Data.Serth.Serialiser.Format.JSON as R
import FlatParse.Basic hiding (Parser)
import GHC.Generics
import Lib
import Network.HTTP (getResponseBody, mkRequest, replaceHeader, simpleHTTP)
import Network.HTTP.Base (Request (..), RequestMethod (..))
import Network.HTTP.Headers (Header (..), HeaderName (..))
import Network.URI (parseURI)
import Network.Wai.Handler.Warp
import Servant hiding (GET, Header, JSON)
import qualified Servant.API as S
import Text.Printf (printf)

instance Accept R.JSON where
    contentTypes _ = contentTypes (Proxy :: Proxy S.JSON)

instance (Serialisable R.JSON a) => MimeRender R.JSON a where
    mimeRender _ = fromStrict . (serialise @R.JSON)

instance (R.FromJSON a) => MimeUnrender R.JSON a where
    mimeUnrender _ bs = case runParser R.parse (toStrict bs) of
        (OK a _) -> Right a
        _ -> Left "Unexpected input"

--
type API =
    ("aeson" :> "user" :> ReqBody '[S.JSON] UserDTO :> Get '[S.JSON] User)
        :<|> ("aeson" :> "list" :> Capture "n" Int :> Get '[S.JSON] [User])
        :<|> ("refl" :> "user" :> ReqBody '[R.JSON] UserDTO :> Get '[R.JSON] User)
        :<|> ("refl" :> "list" :> Capture "n" Int :> Get '[R.JSON] [User])

saveUser :: UserDTO -> User
saveUser (UserDTO n mail isEnabled) = User 1 n mail isEnabled

getUsers :: Int -> [User]
getUsers n = replicate n (User 1 "John Doe" "john@doe.com" True)

server_ :: Server API
server_ = toH saveUser :<|> toH getUsers :<|> toH saveUser :<|> toH getUsers
  where
    toH f = return . f

serverAPI :: Proxy API
serverAPI = Proxy

app :: Application
app = serve serverAPI server_

main :: IO ()
main = do
    thread <- forkIO $ run 8081 app
    defaultMain
        [ bgroup "object" $ benchUser <$> [Aeson, Reflection]
        , bgroup "list" $ concat $ [5, 10, 50] <&> \i -> benchList i <$> [Aeson, Reflection]
        ]
    killThread thread

data Tool = Aeson | Reflection

instance Show Tool where
    show Aeson = "aeson"
    show Reflection = "refl"

parseWithTool :: (Aeson.FromJSON a, R.FromJSON a) => Tool -> ByteString -> a
parseWithTool tool bs = case tool of
    Aeson -> fromMaybe (error "Bad response") $ Aeson.decode bs
    Reflection -> case runParser R.parse (toStrict bs) of
        (OK a _) -> a
        _ -> error "Bad response"

serialiseWithTool :: (Aeson.ToJSON a, Serialisable JSON a) => Tool -> a -> ByteString
serialiseWithTool tool a = case tool of
    Aeson -> encode a
    Reflection -> fromStrict $ serialise @JSON a

benchUser :: Tool -> Benchmark
benchUser tool =
    bench (show tool) $
        whnfAppIO
            (simpleHTTP . buildReq tool "user" . serialiseWithTool tool >=> (getResponseBody >=> return . parseWithTool @User tool))
            (UserDTO "John" "john@doe.con" True)

benchList :: Int -> Tool -> Benchmark
benchList i tool =
    bgroup
        (show i)
        [ bench (show tool) $
            whnfAppIO
                (simpleHTTP >=> (getResponseBody >=> return . parseWithTool @[User] tool))
                (buildReq tool (printf "list/%d" i) BS.empty)
        ]

buildReq :: Tool -> String -> ByteString -> Request ByteString
buildReq tool endpoint body = case parseURI (printf "http://localhost:8081/%s/%s" (show tool) endpoint) of
    Just uri ->
        replaceHeader HdrContentLength (show $ BS.length body) $
            replaceHeader HdrContentType "application/json" (mkRequest @ByteString GET uri){rqBody = body}
    _ -> error "Parsing URL failed"

deriving instance Generic (Request a)
deriving instance Generic RequestMethod
deriving instance Generic Header
deriving instance Generic HeaderName
instance (NFData a) => NFData (Request a)
instance NFData RequestMethod
instance NFData Header
instance NFData HeaderName
