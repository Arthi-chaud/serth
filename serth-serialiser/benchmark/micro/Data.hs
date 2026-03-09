{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data where

import Data.Serth.Serialiser
import Data.Serth.Serialiser.Format.JSON
import Data.Serth.Serialiser.Format.XML
import Data.Serth.Serialiser.Serialisable.TH
import GHC.Generics

data MyRecord = MyRecord {a :: !Int, b :: !String, c :: !String, d :: !Int} deriving (Generic)

newtype MyArrayRecord = MAR {array :: [Int]} deriving (Show, Eq, Generic)

data MySuperRecord = MSPR {number :: Int, nested :: MyArrayRecord} deriving (Show, Eq, Generic)

newtype MySuperArrayRecord = MSPAR {nestedArray :: [MySuperRecord]} deriving (Show, Eq, Generic)

data TreeRecord = TR {record :: !(Either NodeRecord LeafRecord)} deriving (Show, Eq, Generic)

data NodeRecord = NR {left :: !TreeRecord, right :: !TreeRecord} deriving (Show, Eq, Generic)

data LeafRecord = LR {value :: !Int} deriving (Show, Eq, Generic)

data Tree = L !Int | N !Tree !Tree deriving (Show, Eq, Generic)

instance {-# OVERLAPPING #-} (Serialisable JSON a, Serialisable JSON b) => Serialisable JSON (Either a b) where
    builder ctx (Left l) = builder @JSON ctx l
    builder ctx (Right r) = builder @JSON ctx r

genSerialisable @JSON ''MyRecord
genSerialisable @JSON ''MyArrayRecord
genSerialisable @JSON ''MySuperRecord
genSerialisable @JSON ''MySuperArrayRecord
genSerialisables @JSON [''LeafRecord, ''NodeRecord, ''TreeRecord]
genSerialisable @JSON ''Tree

genSerialisable @XML ''MyRecord
genSerialisable @XML ''MyArrayRecord
genSerialisable @XML ''MySuperRecord
genSerialisable @XML ''MySuperArrayRecord
