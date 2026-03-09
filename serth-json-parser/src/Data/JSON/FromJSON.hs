{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.JSON.FromJSON (FromJSON (..)) where

import Data.JSON.Parser.Array (parseArray)
import Data.JSON.Parser.Base
import FlatParse.Basic hiding (Parser)

class FromJSON a where
    parse :: Parser a

instance {-# OVERLAPPING #-} FromJSON String where
    {-# INLINE parse #-}
    parse = parseString

instance FromJSON Double where
    {-# INLINE parse #-}
    parse = parseDouble

instance (FromJSON a) => FromJSON (Maybe a) where
    {-# INLINE parse #-}
    parse = parseNull <|> (Just <$> parse @a)

instance (FromJSON a) => FromJSON [a] where
    {-# INLINE parse #-}
    parse = parseArray parse

instance FromJSON Bool where
    {-# INLINE parse #-}
    parse = parseBool

instance FromJSON Int where
    {-# INLINE parse #-}
    parse = parseIntegral

instance FromJSON Integer where
    {-# INLINE parse #-}
    parse = parseIntegral
