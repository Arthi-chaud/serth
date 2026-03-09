{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module JSON.Simple (benchmark) where

import qualified ByteString.StrictBuilder as Strict
import Criterion.Main
import Data
import Data.Aeson
import Data.ByteString (ByteString, toStrict)
import Data.ByteString.Builder (Builder, byteString, toLazyByteString)
import Data.Functor
import Data.Serth.Serialiser
import Data.Serth.Serialiser.Format.JSON

instance FromJSON MyRecord
instance ToJSON MyRecord

benchmark :: Benchmark
benchmark =
    bgroup
        "simple"
        ( [("verysmall", verySmallRec), ("small", smallRec), ("big", bigRec)]
            <&> ( \(name, rec) ->
                    bgroup
                        name
                        [ bench "aeson" $ nf encodeWithAeson rec
                        , bench "naive/lazy" $ nf encodeWithTemplateLazy rec
                        , bench "naive/strict" $ nf encodeWithTemplateLazy rec
                        , bench "futadata" $ nf (serialise @JSON) rec
                        ]
                )
        )
  where
    !verySmallRec = MyRecord 1 "a" "b" 3
    !smallRec = MyRecord 10000 (replicate 100 'a') (replicate 20 'b') 3
    !bigRec = MyRecord 1 (replicate 1000 'a') (replicate 1000 'b') 10000

encodeWithAeson :: MyRecord -> ByteString
encodeWithAeson = toStrict . encode

encodeWithTemplateLazy :: MyRecord -> ByteString
encodeWithTemplateLazy rec =
    let
        abuilder = builderLazy rec.a
        bbuilder = builderLazy rec.b
        cbuilder = builderLazy rec.c
        dbuilder = builderLazy rec.d
     in
        toStrict . toLazyByteString $
            byteString (naiveTemplate !! 0)
                <> abuilder
                <> byteString (naiveTemplate !! 1)
                <> bbuilder
                <> byteString (naiveTemplate !! 2)
                <> cbuilder
                <> byteString (naiveTemplate !! 3)
                <> dbuilder
                <> byteString (naiveTemplate !! 4)

{-# INLINE builderLazy #-}
builderLazy :: (ToJSON a) => a -> Builder
builderLazy = fromEncoding . toEncoding

encodeWithTemplateStrict :: MyRecord -> ByteString
encodeWithTemplateStrict rec =
    let
        abuilder = builderStrict rec.a
        bbuilder = builderStrict rec.b
        cbuilder = builderStrict rec.c
        dbuilder = builderStrict rec.d
     in
        Strict.builderBytes $
            Strict.bytes (naiveTemplate !! 0)
                <> abuilder
                <> Strict.bytes (naiveTemplate !! 1)
                <> bbuilder
                <> Strict.bytes (naiveTemplate !! 2)
                <> cbuilder
                <> Strict.bytes (naiveTemplate !! 3)
                <> dbuilder
                <> Strict.bytes (naiveTemplate !! 4)

{-# INLINE builderStrict #-}
builderStrict :: (ToJSON a) => a -> Strict.Builder
builderStrict = Strict.lazyBytes . encode

naiveTemplate :: [ByteString]
naiveTemplate = ["{\"a\":", ",\"b\":", ",\"c\":", ",\"d\":", "}"]
