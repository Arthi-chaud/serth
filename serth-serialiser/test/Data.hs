{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Data where

import Data.Serth.Serialiser
import Data.Serth.Serialiser.Format.JSON
import Data.Serth.Serialiser.Format.XML

import GHC.Generics (Generic)

newtype MySimpleRecord = MSR {a :: Int} deriving (Show, Eq, Generic)

newtype MyArrayRecord = MAR {array :: [Int]} deriving (Show, Eq, Generic)

data MySuperRecord = MSPR {number :: Int, nested :: MyArrayRecord} deriving (Show, Eq, Generic)

newtype MySuperArrayRecord = MSPAR {nestedArray :: [MySuperRecord]} deriving (Show, Eq, Generic)

data MyEither = MyNull | MyLeft Int | MyRight String Int deriving (Show, Eq, Generic)

genSerialisable @JSON ''MySimpleRecord
genSerialisable @JSON ''MyArrayRecord
genSerialisable @JSON ''MySuperRecord
genSerialisable @JSON ''MySuperArrayRecord
genSerialisable @JSON ''MyEither

genSerialisable @XML ''MySimpleRecord
genSerialisable @XML ''MyArrayRecord
genSerialisable @XML ''MySuperRecord
genSerialisable @XML ''MySuperArrayRecord
genSerialisable @XML ''MyEither
