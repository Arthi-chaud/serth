{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Data where

import Control.DeepSeq
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Serth.Serialiser (genSerialisable)
import Data.Serth.Serialiser.Format.JSON
import GHC.Generics
import qualified MB

data Page = Page
    { created :: String
    , count :: Int
    , offset :: Int
    , labels :: [Label]
    }
    deriving (Generic, Show)

type MBID = String
data LabelType
    = Imprint
    deriving (Generic, Show)

translateLabelType :: MB.LabelType -> LabelType
translateLabelType _ = Imprint

data AreaType = Country deriving (Generic, Show)

translateAreaType :: MB.AreaType -> AreaType
translateAreaType _ = Country

data Tag = Tag {count :: Int, name :: String} deriving (Generic, Show)

translateTag :: MB.Tag -> Tag
translateTag (MB.Tag n s) = Tag n s

data Lifespan = Lifespan
    { ended :: Maybe Bool
    , begin :: Maybe String
    , end :: Maybe String
    }
    deriving (Generic, Show)

translateLifespan :: MB.Lifespan -> Lifespan
translateLifespan (MB.Lifespan ee b e) = Lifespan ee b e

data Alias = Alias
    { name :: String
    , sortName :: String
    , locale :: Maybe String
    }
    deriving (Generic, Show)

translateAlias :: MB.Alias -> Alias
translateAlias (MB.Alias n s l) = Alias n s l

data Area = Area
    { id :: MBID
    , areaType :: AreaType
    , areaTypeId :: MBID
    , name :: String
    , sortName :: String
    , lifeSpan :: Lifespan
    }
    deriving (Generic, Show)

translateArea :: MB.Area -> Area
translateArea (MB.Area mbid at tmbid s s1 ls) =
    Area
        mbid
        (translateAreaType at)
        tmbid
        s
        s1
        (translateLifespan ls)

data Label = Label
    { id :: MBID
    , labelType :: Maybe LabelType
    , labelTypeId :: MBID
    , score :: Int
    , name :: String
    , sortName :: String
    , labelCode :: Int
    , disambiguation :: Maybe String
    , country :: String
    , area :: Maybe Area
    , lifeSpan :: Lifespan
    , aliases :: [Alias]
    , tags :: [Tag]
    }
    deriving (Generic, Show)

translateLabel :: MB.Label -> Label
translateLabel (MB.Label mbid lt ltid scr n sn lc dis c a ls as ts) =
    Label
        mbid
        (translateLabelType <$> lt)
        (fromMaybe "00000-00000-00000-00000" ltid)
        scr
        n
        sn
        (fromMaybe 0 lc)
        dis
        (fromMaybe "" c)
        (translateArea <$> a)
        (translateLifespan ls)
        (translateAlias <$> as)
        (translateTag <$> ts)

translateDataSet :: [MB.Label] -> [Label]
translateDataSet = fmap translateLabel

instance ToJSON Page
instance ToJSON Area
instance ToJSON Label
instance ToJSON Alias
instance ToJSON AreaType
instance ToJSON LabelType
instance ToJSON Lifespan
instance ToJSON Tag

instance NFData Page
instance NFData Area
instance NFData Label
instance NFData Alias
instance NFData AreaType
instance NFData LabelType
instance NFData Lifespan
instance NFData Tag

genSerialisable @JSON ''Tag
genSerialisable @JSON ''Lifespan
genSerialisable @JSON ''AreaType
genSerialisable @JSON ''LabelType
genSerialisable @JSON ''Area
genSerialisable @JSON ''Alias
genSerialisable @JSON ''Label
genSerialisable @JSON ''Page
