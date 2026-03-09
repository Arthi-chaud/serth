{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module Data.JSON.Parser.Internal (liftB, liftS, skipSpacesS, skipSpacesB) where

import qualified FlatParse.Basic as B

import Data.Char
import qualified FlatParse.Stateful as S
import GHC.Exts

{-# INLINE skipSpacesS #-}
skipSpacesS :: S.ParserT st r e ()
skipSpacesS = S.skipMany $ S.satisfyAscii isSpace

{-# INLINE skipSpacesB #-}
skipSpacesB :: B.Parser err ()
skipSpacesB = B.skipMany $ B.satisfyAscii isSpace

-- | Runs a Basic parser in a stateful one
{-# INLINE liftB #-}
liftB :: B.Parser err a -> S.Parser s err a
liftB p = S.ParserT $ \(!fptr) !_ !addr1 !addr2 !i !st ->
    case B.runParserT# p fptr addr1 addr2 st of
        B.OK# st' res addr -> S.OK# st' res addr i
        B.Err# st' e -> S.Err# st' e
        B.Fail# st' -> S.Fail# st'

-- | Runs a stateful parser in a basic one
liftS :: S.Parser reader err res -> reader -> Int -> B.Parser err res
liftS parser state (I# i) = B.ParserT $ \(!fptr) !addr1 !addr2 !st ->
    case S.runParserT# parser fptr state addr1 addr2 i st of
        S.OK# st' res addr _ -> B.OK# st' res addr
        S.Err# st' e -> B.Err# st' e
        S.Fail# st' -> B.Fail# st'
