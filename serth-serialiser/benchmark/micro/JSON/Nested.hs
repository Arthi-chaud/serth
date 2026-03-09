{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module JSON.Nested (benchmark) where

import Criterion.Main
import Data
import Data.Aeson
import Data.ByteString (ByteString, toStrict)
import Data.ByteString.Builder
import Data.Functor
import Data.Serth.Serialiser
import Data.Serth.Serialiser.Format.JSON

instance ToJSON MyArrayRecord
instance ToJSON MySuperRecord
instance ToJSON MySuperArrayRecord
instance ToJSON TreeRecord
instance ToJSON NodeRecord
instance ToJSON LeafRecord
instance ToJSON Tree

benchmark :: Benchmark
benchmark =
    bgroup
        "nested"
        [ bgroup
            "object-w-array"
            $ let rec = MSPR 100000 $ MAR (replicate 100 100)
                  manualInlineCase = bench "futadata-manual-inline" $ nf objectWithArrayManualInline rec
               in (cases rec ++ [manualInlineCase])
        , bgroup
            "array-of-object"
            $ cases
                (MSPAR $ replicate 1000 (MSPR 1 $ MAR [0 .. 10]))
        , bgroup
            "recursive"
            [ bgroup "record" $
                [1, 5, 10, 15 :: Int]
                    <&> ( \depth ->
                            bgroup (show depth) $
                                cases $
                                    mkTreeRecord depth
                        )
            , bgroup "non-record" $
                [1, 5, 10, 15 :: Int]
                    <&> ( \depth ->
                            bgroup (show depth) $
                                cases $
                                    mkTree depth
                        )
            ]
        ]
  where
    mkTreeRecord 0 = TR $ Right $ LR 0
    mkTreeRecord n = let !sub = mkTreeRecord (n - 1) in TR $ Left $ NR sub sub
    mkTree 0 = L 0
    mkTree n = let !sub = mkTree (n - 1) in N sub sub
    cases !rec =
        [ bench "aeson" $ nf (toStrict . encode) rec
        , bench "futadata" $ nf (serialise @JSON) rec
        ]

    objectWithArrayManualInline :: MySuperRecord -> ByteString
    objectWithArrayManualInline rec =
        let
            nBuilder = builder @JSON [] $ number rec
            arrayBuilder = builder @JSON [] $ array $ nested rec
         in
            toStrict . toLazyByteString $
                byteString (template !! 0)
                    <> nBuilder
                    <> byteString (template !! 1)
                    <> arrayBuilder
                    <> byteString (template !! 2)
      where
        template = ["{\"number\":", "\"nested\": {\"array\":", "}}"]
