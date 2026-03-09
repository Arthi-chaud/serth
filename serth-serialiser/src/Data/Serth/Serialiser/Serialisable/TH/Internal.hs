{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Serth.Serialiser.Serialisable.TH.Internal where

import Data.ByteString.Builder
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Data
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Serth.Serialiser.FieldName
import Data.Serth.Serialiser.Format
import Data.Serth.Serialiser.Serialisable
import Data.Serth.Serialiser.Serialisable.TH.CompiledTemplate
import Data.Serth.Serialiser.Serialisable.TH.Options
import Data.Serth.Serialiser.Template
import Data.Serth.Base.Type
import Language.Haskell.TH (Name, mkName, nameBase)

-- | Generates a template from a given 'DataConstructor'.
--
-- It takes as parameter the number of siblings constructors, i.e. the number of constructor the parent type has, minus one
templateFromConstructor :: forall format. (Format format) => Options -> Int -> Constructor -> Template format
templateFromConstructor opts siblings dc =
    objectTemplate @format $
        (\(fname, hole) -> (fname, Hole fname hole))
            <$> templateArgFromConstructor @format dc siblings opts

templateArgFromConstructor :: forall format. (Format format) => Constructor -> Int -> Options -> [(FieldName, TemplateHole)]
-- If record with 1 constructor: simply encode
templateArgFromConstructor (RecordCons _ fieldMap) 0 _ =
    zip [0 ..] fieldMap
        <&> ( \(idx, (fName, _)) ->
                let
                    n = nameToFieldName fName
                 in
                    (n, Field idx)
            )
-- If record with more than 1 constructor, add field with name of the constructor
-- See https://hackage.haskell.org/package/aeson-2.2.3.0/docs/Data-Aeson.html#v:TaggedObject
templateArgFromConstructor dc@(RecordCons _ _) _ opts = case sumEncoding opts of
    TaggedObject tagFieldName _ ->
        let recordFields = templateArgFromConstructor @format dc 0 opts
         in (FieldName . pack $ tagFieldName, ConName) : recordFields
-- If non-record/fields have no name, mk a 2-field record, one for the tag, one for the content
templateArgFromConstructor (SimpleCons _ f) _ opts = case sumEncoding opts of
    TaggedObject tagFieldName contentFieldName ->
        [ (FieldName (pack tagFieldName), ConName)
        , (FieldName (pack contentFieldName), Fields [0 .. length f - 1])
        ]

-- | Returns a 'Format'\'s 'Name'
getFormatName :: forall format. (Format format) => Name
getFormatName =
    let
        rep = typeRepTyCon $ typeRep (Proxy :: Proxy format)
     in
        mkName $ intercalate "." [tyConModule rep, tyConName rep]

resolveConNameHoles :: forall format. (Serialisable format String) => String -> CompiledTemplate -> CompiledTemplate
resolveConNameHoles consName template = optimiseTemplate $ go template
  where
    go :: CompiledTemplate -> CompiledTemplate
    go [] = []
    go (Left (fName, ConName) : rest) =
        Right (L.unpack (toLazyByteString (builder @format @String [fName] consName))) : go rest
    go (x : xs) = x : go xs

nameToFieldName :: Name -> FieldName
nameToFieldName = FieldName . pack . nameBase
