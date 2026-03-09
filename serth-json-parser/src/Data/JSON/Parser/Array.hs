{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.JSON.Parser.Array (parseArray) where

import Data.JSON.Parser.Base
import Data.JSON.Parser.Internal
import FlatParse.Basic hiding (Parser)

{-# INLINE parseArray #-}
parseArray :: Parser a -> Parser [a]
parseArray parser = do
    $(char '[') <|> err (ExpectedValueType "list")
    skipSpacesB
    withAnyResult
        ($(char ']'))
        (const $ pure [])
        ( do
            !items <- go
            skipSpacesB
            $(char ']')
            return items
        )
        err
  where
    {-# INLINE go #-}
    go = do
        a <- parser
        skipSpacesB
        withAnyResult
            ($(char ','))
            (const $ (a :) <$> (skipSpacesB >> go))
            (return [a])
            err
