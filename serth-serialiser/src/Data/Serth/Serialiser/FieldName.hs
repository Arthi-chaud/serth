module Data.Serth.Serialiser.FieldName (FieldName (..)) where

import Data.ByteString (ByteString)

-- | Represents the name of a field in a record to serialise
newtype FieldName = FieldName {unFieldName :: ByteString} deriving (Show, Eq)
