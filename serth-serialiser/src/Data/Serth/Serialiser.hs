module Data.Serth.Serialiser (
    -- * Serialisable objects
    Serialisable (..),
    serialise,

    -- * Code generator
    genSerialisable,

    -- * Definition of a serialisation format
    Format,
) where

import Data.Serth.Serialiser.Format
import Data.Serth.Serialiser.Serialisable
import Data.Serth.Serialiser.Serialisable.TH
