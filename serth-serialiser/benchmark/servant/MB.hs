{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module MB where

import Control.Monad
import Data.Aeson
import GHC.Generics (Generic)

type MBID = String

data LabelType
    = Imprint
    | LabelName
    | Distributor
    | OriginalProduction
    | Production
    | ReissueProduction
    | OtherLabelType String
    deriving (Show)

data AreaType
    = Country
    | OtherAreaType String
    deriving (Show)

data Tag = Tag
    { countTag :: Int
    , nameTag :: String
    }
    deriving (Show, Generic)

data Lifespan = Lifespan
    { ended :: Maybe Bool
    , begin :: Maybe String
    , end :: Maybe String
    }
    deriving (Show, Generic)

data Area = Area
    { idArea :: MBID
    , areaType :: AreaType
    , areaTypeId :: MBID
    , nameArea :: String
    , sortNameArea :: String
    , lifeSpanArea :: Lifespan
    }
    deriving (Show, Generic)

data Alias = Alias
    { aliasName :: String
    , aliasSortName :: String
    , aliasLocale :: Maybe String
    }
    deriving (Show, Generic)

data Label = Label
    { idLabel :: MBID
    , labelType :: Maybe LabelType
    , labelTypeId :: Maybe MBID
    , score :: Int
    , nameLabel :: String
    , sortNameLabel :: String
    , labelCode :: Maybe Int
    , disambiguation :: Maybe String
    , country :: Maybe String
    , area :: Maybe Area
    , lifeSpan :: Lifespan
    , aliases :: [Alias]
    , tags :: [Tag]
    }
    deriving (Show, Generic)

instance FromJSON Label where
    parseJSON = withObject "Label" $ \v ->
        Label
            <$> v .: "id"
            <*> v .:? "type"
            <*> v .:? "type-id"
            <*> v .: "score"
            <*> v .: "name"
            <*> v .: "sort-name"
            <*> v .:? "label-code"
            <*> v .:? "disambiguation"
            <*> v .:? "country"
            <*> v .:? "area"
            <*> v .: "life-span"
            <*> v .:? "aliases" .!= []
            <*> v .:? "tags" .!= []

instance FromJSON Tag where
    parseJSON = withObject "Tag" $ \v ->
        Tag
            <$> v .: "count"
            <*> v .: "name"

instance FromJSON Lifespan where
    parseJSON = withObject "Lifespan" $ \v ->
        Lifespan
            <$> v .:? "ended"
            <*> v .:? "begin"
            <*> v .:? "end"

instance FromJSON Area where
    parseJSON = withObject "Area" $ \v ->
        Area
            <$> v .: "id"
            <*> v .: "type"
            <*> v .: "type-id"
            <*> v .: "name"
            <*> v .: "sort-name"
            <*> v .: "life-span"

instance FromJSON Alias where
    parseJSON = withObject "Alias" $ \v ->
        Alias
            <$> v .: "name"
            <*> v .: "sort-name"
            <*> v .:? "locale"

--------------------------------------------------
-- Custom parsing for sum types
--------------------------------------------------

instance FromJSON LabelType where
    parseJSON = withText "LabelType" $ \t ->
        pure $ case t of
            "Imprint" -> Imprint
            "Label name" -> LabelName
            "Distributor" -> Distributor
            "Original Production" -> OriginalProduction
            "Production" -> Production
            "Reissue Production" -> ReissueProduction
            other -> OtherLabelType (show other)

instance FromJSON AreaType where
    parseJSON = withText "AreaType" $ \t ->
        pure $ case t of
            "Country" -> Country
            other -> OtherAreaType (show other)

loadDataSet :: FilePath -> IO [Label]
loadDataSet = eitherDecodeFileStrict >=> either fail pure
