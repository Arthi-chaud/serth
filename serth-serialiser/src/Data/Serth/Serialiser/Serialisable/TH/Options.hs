module Data.Serth.Serialiser.Serialisable.TH.Options (Options (..), SumEncoding (..), defaultOptions) where

newtype Options = Options
    { sumEncoding :: SumEncoding
    }

data SumEncoding
    = TaggedObject
        -- | Name of the field that contains the constructor tag
        String
        -- | Name of the field that contains the values of the constructor
        String

-- TODO SingleField, 2-elem array, untagged

defaultOptions :: Options
defaultOptions = Options $ TaggedObject "tag" "contents"
