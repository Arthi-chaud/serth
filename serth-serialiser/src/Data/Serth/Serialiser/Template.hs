{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Data.Serth.Serialiser.Template (
    Template (..),
    TemplateHole (..),
    join,
    between,
) where

import Data.ByteString (ByteString)
import Data.List (intersperse)
import Data.Serth.Serialiser.FieldName (FieldName)

-- | A template is an AST that allows building a bytestring template at compile time
--
-- The type parameter symbolises the layout. It is usually set to
data Template format
    = -- | A single character as a token
      Token Char
    | Tokens ByteString
    | -- | Concatenate templates
      And (Template format) (Template format)
    | -- | A hole in the template which will be filled by a value know at serialisation time
      Hole FieldName TemplateHole
    | -- | An empty template
      Empty
    deriving (Show)

instance Semigroup (Template format) where
    {-# INLINE (<>) #-}
    (<>) = And

instance Monoid (Template format) where
    {-# INLINE mempty #-}
    mempty = Empty

join :: Template f -> [Template f] -> Template f
join inter = mconcat . intersperse inter

-- | Wraps the characters around the givent template
between :: Char -> Char -> Template a -> Template a
between left right body = Token left <> body <> Token right

-- | Specifies what the content of a hole should be
data TemplateHole
    = -- | A single field's value, using its position in the constructor's declaration
      Field Int
    | -- | Multiple fields' values, using their position in the constructor's declaration
      Fields [Int]
    | -- | The name of the constructor
      ConName
    deriving (Eq, Show)
