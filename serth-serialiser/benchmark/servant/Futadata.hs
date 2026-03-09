{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- https://hackage.haskell.org/package/servant-xml-1.0.3/docs/src/Servant.XML.html

module Futadata () where

import Data.Aeson (FromJSON)
import Data.ByteString (fromStrict)
import qualified Data.Serth.Serialiser as Futa
import qualified Data.Serth.Serialiser.Format.JSON as Futa
import Data.Proxy
import Servant.API (Accept (contentTypes), JSON, MimeRender (..), MimeUnrender (mimeUnrender))

instance Accept Futa.JSON where
    contentTypes _ = contentTypes (Proxy :: Proxy JSON)

instance (Futa.Serialisable Futa.JSON a) => MimeRender Futa.JSON a where
    mimeRender _ = fromStrict . (Futa.serialise @Futa.JSON)

instance (FromJSON a) => MimeUnrender Futa.JSON a where
    mimeUnrender _ = mimeUnrender (Proxy :: Proxy JSON)
