{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Serth.Serialiser.Format (Format (..)) where

import Data.ByteString (ByteString)
import Data.Serth.Serialiser.FieldName
import Data.Serth.Serialiser.Template
import Data.Typeable (Typeable)

-- | Typeclass for Serialisation formats
class (Typeable f) => Format f where
    -- | Produces a template based on the map of field names to their values, represented as 'Hole's
    objectTemplate :: [(FieldName, Template f)] -> Template f

    -- | Returns a triple, containing:
    --
    -- - The leading static element at the beginning of the list
    -- - The static element that goes between each element of the list
    -- - The trailing static element at the end of the list
    --
    -- For examples, see 'Data.Serth.Serialiser.Format.JSON' and 'Data.Serth.Serialiser.Format.XML'
    --
    -- The list parameters represents the list of parent field names, as a stack
    arrayTemplate :: [FieldName] -> (ByteString, ByteString, ByteString)

    -- | Formats a 'FieldName' to be inserted in a template.
    --
    -- For examples, see 'Data.Serth.Serialiser.Format.JSON' and 'Data.Serth.Serialiser.Format.XML'
    fieldName :: FieldName -> Template f
