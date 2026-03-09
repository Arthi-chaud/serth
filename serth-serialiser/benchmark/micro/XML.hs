{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module XML (benchmark) where

import Criterion.Main
import Data (MyRecord (..))
import Data.Functor
import Data.Serth.Serialiser
import Data.Serth.Serialiser.Format.XML
import Text.XML.Light

benchmark :: Benchmark
benchmark =
    bgroup
        "xml"
        ( [("verysmall", verySmallRec), ("small", smallRec), ("big", bigRec)]
            <&> ( \(name, rec) ->
                    bgroup
                        name
                        [ bench "xml" $ nf (ppElement . node blank_name) rec
                        , bench "futadata" $ nf (serialise @XML) rec
                        ]
                )
        )
  where
    !verySmallRec = MyRecord 1 "a" "b" 3
    !smallRec = MyRecord 10000 (replicate 100 'a') (replicate 20 'b') 3
    !bigRec = MyRecord 1 (replicate 1000 'a') (replicate 1000 'b') 10000

instance Node MyRecord where
    node qn rec =
        node
            qn
            [ unode "a" $ show $ a rec
            , unode "b" $ b rec
            , unode "c" $ c rec
            , unode "d" $ show $ d rec
            ]
