{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.Serth.Serialiser.Format.XML (XML) where

import qualified Data.ByteString as BS
import Data.ByteString.Builder
import Data.ByteString.Char8 (empty, pack, unpack)
import qualified Data.ByteString.Lazy as BL
import Data.List (intersperse)
import Data.Maybe (listToMaybe)
import Data.Serth.Serialiser.FieldName
import Data.Serth.Serialiser.Format
import Data.Serth.Serialiser.Serialisable
import Data.Serth.Serialiser.Template

-- | Witness that a template is for XML
data XML

instance (Integral a, Show a) => Serialisable XML a where
    {-# INLINE builder #-}
    builder _ = string8 . show

instance {-# OVERLAPPING #-} Serialisable XML String where
    {-# INLINE builder #-}
    builder _ = string8

instance {-# OVERLAPPING #-} (Serialisable XML a) => Serialisable XML [a] where
    {-# INLINE builder #-}
    builder ctx = mconcat . intersperse separator . map (builder @XML ctx)
      where
        separator = byteString "</" <> fName <> byteString "><" <> fName <> char7 '>'
        fName = byteString $ maybe empty unFieldName $ listToMaybe ctx

instance {-# OVERLAPPING #-} Serialisable XML BS.ByteString where
    {-# INLINE builder #-}
    builder _ = byteString

instance {-# OVERLAPPING #-} Serialisable XML BL.ByteString where
    {-# INLINE builder #-}
    builder _ = lazyByteString

instance Format XML where
    fieldName = Tokens . unFieldName
    arrayTemplate ctx = (pack leading, pack $ trailing ++ leading, pack trailing)
      where
        fName = maybe "" (unpack . unFieldName) $ listToMaybe ctx
        leading = "<" ++ fName ++ ">"
        trailing = "</" ++ fName ++ ">"
    objectTemplate =
        mconcat
            . map
                ( \(fName, hole) -> case hole of
                    Hole _ (Fields []) -> Empty
                    Hole _ (Fields _) -> hole
                    _ ->
                        between '<' '>' (fieldName @XML fName)
                            <> hole
                            <> Tokens "</"
                            <> fieldName @XML fName
                            <> Token '>'
                )
