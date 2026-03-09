{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.JSON.Parser.Base (
    Parser,
    Error (..),
    parseBool,
    parseNull,
    parseString,
    parseDouble,
    parseIntegral,

    -- * From Flatparse
    runParser,
    Result (..),
) where

import Control.Monad (replicateM)
import Data.Char (chr, digitToInt, isHexDigit, readLitChar)
import Data.List.NonEmpty (NonEmpty)
import FlatParse.Basic hiding (Parser)
import qualified FlatParse.Basic as FP
import GHC.Float (int2Double)

data Error
    = ExpectedValueType String
    | ExpectedToken Char
    | MissingDecimalPart
    | MissingFields (NonEmpty String)
    deriving (Eq, Show)

type Parser a = FP.Parser Error a

{-# INLINE parseBool #-}
parseBool :: Parser Bool
parseBool =
    $( FP.switch
        [|
            case _ of
                "true" -> pure True
                "false" -> pure False
            |]
     )
        <|> err (ExpectedValueType "boolean")

{-# INLINE parseNull #-}
parseNull :: Parser (Maybe a)
parseNull = Nothing <$ $(string "null")

{-# INLINE parseString #-}
parseString :: Parser String
parseString = do
    parseQuote <|> err (ExpectedValueType "string")
    s <- many parseStringChar
    parseQuote
    return s
  where
    {-# INLINE parseQuote #-}
    parseQuote = $(char '"')
    {-# INLINE parseStringChar #-}
    parseStringChar :: Parser Char
    parseStringChar = do
        c <- satisfyAscii (/= '"')
        case c of
            '\\' -> parseEscaped
            c' -> return c'
    {-# INLINE parseEscaped #-}
    parseEscaped :: Parser Char
    parseEscaped = (chr <$> parseEscapedAsciiCode) <|> parseEscapedChar
    {-# INLINE parseEscapedAsciiCode #-}
    parseEscapedAsciiCode :: Parser Int
    parseEscapedAsciiCode = do
        $(char 'u')
        hexs <- replicateM 4 $ satisfyAscii isHexDigit
        return $ getNbfromHex hexs
    parseEscapedChar =
        satisfyAscii (== '/') <|> do
            c <- satisfyAscii (`elem` "bfnrt\\\"")
            return $ fst $ head $ readLitChar ('\\' : [c])

    {-# INLINE getNbfromHex #-}
    getNbfromHex = foldl (\(!b) (!c) -> b * 16 + digitToInt c) 0

{-# INLINE parseIntegral #-}
parseIntegral :: (Integral a, Read a) => Parser a
parseIntegral =
    withOption
        ($(char '-'))
        (const $ parser True)
        (parser False)
  where
    {-# INLINE parser #-}
    parser isNeg = do
        n <- (fromInteger <$> anyAsciiDecimalInteger) <|> err (ExpectedValueType "integer")
        return $ if isNeg then -n else n

{-# INLINE parseDouble #-}
parseDouble :: Parser Double
parseDouble =
    withOption
        ($(char '-'))
        (const $ parser True)
        (parser False)
  where
    {-# INLINE parser #-}
    parser isNeg = do
        d <- parsePositiveDouble
        return $ if isNeg then -d else d
    {-# INLINE parsePositiveDouble #-}
    parsePositiveDouble = do
        integralPart <- int2Double <$> anyAsciiDecimalInt
        withAnyResult
            ($(char '.'))
            (\() -> (integralPart +) <$> parseDecimal 0 0)
            (return integralPart)
            err
    {-# INLINE parseDecimal #-}
    parseDecimal :: Int -> Int -> Parser Double
    parseDecimal acc digitAfterComa = do
        digit <-
            ( digitToInt
                <$> satisfyAscii isDigit
            )
                <|> if digitAfterComa == 0 then err MissingDecimalPart else failed
        let res = acc * 10 + digit
        parseDecimal res (digitAfterComa + 1) <|> return (int2Double res * (10 ^^ (-digitAfterComa - 1)))
