{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Serth.Serialiser.Serialisable (Serialisable (..), serialise) where

import Data.ByteString (ByteString, toStrict)
import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.Serth.Serialiser.FieldName (FieldName)

-- | Typeclass for object that can be serialised
--
-- Usually, for a given format, user is expected to provide instances of
-- this typeclass for anything other than lists and records
--
-- The 'Data.Serth.Serialiser.Compiler' will generate an instance of that typeclass for the given format and object
class Serialisable format a where
    -- | The list of 'FieldName's given as input represents the stack of the nested fields. Thus:
    --
    --  - the first element in the list is the name of the parent fields (as a stack).
    --  - the last element is the top-most field
    --  - If the list is empty, the object to serialise is the root
    builder :: [FieldName] -> a -> Builder

{-# INLINE serialise #-}
serialise :: forall format a. (Serialisable format a) => a -> ByteString
serialise = toStrict . toLazyByteString . builder @format []
