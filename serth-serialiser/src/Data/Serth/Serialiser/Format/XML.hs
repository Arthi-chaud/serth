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
import Data.Map (Map, foldMapWithKey)
import Data.Maybe (listToMaybe)
import Data.Serth.Serialiser.FieldName
import Data.Serth.Serialiser.Format
import Data.Serth.Serialiser.Serialisable
import Data.Serth.Serialiser.Template
import Data.String

-- | Witness that a template is for XML
data XML

instance (Integral a, Show a) => Serialisable XML a where
    {-# INLINE builder #-}
    builder _ = string8 . show

instance {-# OVERLAPPING #-} Serialisable XML Bool where
    {-# INLINE builder #-}
    builder _ b = if b then byteString "true" else byteString "false"

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

instance {-# OVERLAPPING #-} (Serialisable XML a, Serialisable XML b) => Serialisable XML (a, b) where
    {-# INLINE builder #-}
    builder ctx (a, b) =
        builder @XML ctx a
            <> builder @XML ctx b

instance {-# OVERLAPPING #-} (Serialisable XML a, Serialisable XML b, Serialisable XML c) => Serialisable XML (a, b, c) where
    {-# INLINE builder #-}
    builder ctx (a, b, c) =
        builder @XML ctx a
            <> builder @XML ctx b
            <> builder @XML ctx c

instance {-# OVERLAPPING #-} (Serialisable XML a, IsString a, Serialisable XML b) => Serialisable XML (Map a b) where
    {-# INLINE builder #-}
    builder c = foldMapWithKey go
      where
        go key value =
            byteString "<"
                <> builder @XML c key
                <> byteString ">"
                <> builder @XML c value
                <> byteString "</"
                <> builder @XML c key
                <> byteString ">"

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
