{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.Serth.Serialiser.Format.JSON (JSON) where

import Data.Aeson
import Data.Bifunctor
import qualified Data.ByteString as BS
import Data.ByteString.Builder (byteString, char8, lazyByteString)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.List (intersperse)
import Data.Map
import Data.Serth.Serialiser.FieldName
import Data.Serth.Serialiser.Format
import Data.Serth.Serialiser.Serialisable
import Data.Serth.Serialiser.Template
import Data.String (IsString)

-- | Witness that a template is for JSON
data JSON

instance Serialisable JSON Int where
    {-# INLINE builder #-}
    builder _ = fromEncoding . toEncoding

instance Serialisable JSON Float where
    {-# INLINE builder #-}
    builder _ = fromEncoding . toEncoding

instance Serialisable JSON Bool where
    {-# INLINE builder #-}
    builder _ = fromEncoding . toEncoding

instance {-# OVERLAPPING #-} Serialisable JSON String where
    {-# INLINE builder #-}
    builder _ = fromEncoding . toEncoding

instance (Serialisable JSON a, Serialisable JSON b, Serialisable JSON c) => Serialisable JSON (a, b, c) where
    {-# INLINE builder #-}
    builder ctx (a, b, c) =
        char8 '['
            <> builder @JSON ctx a
            <> char8 ','
            <> builder @JSON ctx b
            <> char8 ','
            <> builder @JSON ctx c
            <> char8 ']'

instance (Serialisable JSON a, Serialisable JSON b) => Serialisable JSON (a, b) where
    {-# INLINE builder #-}
    builder ctx (a, b) =
        char8 '['
            <> builder @JSON ctx a
            <> char8 ','
            <> builder @JSON ctx b
            <> char8 ']'

instance (Serialisable JSON a) => Serialisable JSON (Maybe a) where
    {-# INLINE builder #-}
    builder _ Nothing = byteString "null"
    builder ctx (Just a) = builder @JSON ctx a

instance (Serialisable JSON a, IsString a, Serialisable JSON b) => Serialisable JSON (Map a b) where
    {-# INLINE builder #-}
    builder c m = char8 '{' <> go (toList m)
      where
        go ((key, value) : rest) =
            char8 '"'
                <> builder @JSON c key
                <> B.byteString "\":"
                <> builder @JSON c value
                <> if Prelude.null rest then go rest else char8 ',' <> go rest
        go [] = char8 '}'

instance {-# OVERLAPPING #-} forall a. (Serialisable JSON a) => Serialisable JSON [a] where
    {-# INLINE builder #-}
    builder _ items =
        B.char8 '['
            <> mconcat (intersperse (B.char8 ',') (builder @JSON [] <$> items))
            <> B.char8 ']'

instance {-# OVERLAPPING #-} Serialisable JSON BS.ByteString where
    {-# INLINE builder #-}
    builder _ bs = char8 '\"' <> byteString bs <> char8 '\"'

instance {-# OVERLAPPING #-} Serialisable JSON BL.ByteString where
    {-# INLINE builder #-}
    builder _ bs = char8 '\"' <> lazyByteString bs <> char8 '\"'

instance Format JSON where
    objectTemplate fields =
        let
            -- If a hole is an array of fields with 0 fields
            -- We remove the field altogether
            filterFields =
                Prelude.filter
                    ( \(_, hole) -> case hole of
                        (Hole _ (Fields [])) -> False
                        _ -> True
                    )
            -- If a hole is a field list with exactly one element, we remove the brackets
            fieldsToField =
                Prelude.map
                    ( second $ \hole -> case hole of
                        Hole fname (Fields [n]) -> Hole fname (Field n)
                        _ -> hole
                    )
            buildPair (fname, hole) = fieldName @JSON fname <> Token ':' <> hole
         in
            between '{' '}' $
                join
                    (Token ',')
                    (Prelude.map buildPair . fieldsToField . filterFields $ fields)
    arrayTemplate _ = ("[", ",", "]")
    fieldName (FieldName f) = between '"' '"' $ Tokens f
