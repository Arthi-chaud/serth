{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.DeepSeq
import Criterion.Main
import Data.Aeson
import Data.ByteString (toStrict)
import Data.ByteString.Builder (toLazyByteString)
import Data.Functor
import Data.Serth.Serialiser
import Data.Serth.Serialiser.Format.JSON
import Data.Serth.Serialiser.Format.XML
import GHC.Generics (Generic)
import Text.XML.Light

data Tree = MkTree
    { value :: Int
    , left :: Maybe Tree
    , right :: Maybe Tree
    }
    deriving (Generic, Show, Eq)

instance ToJSON Tree
instance NFData Tree
instance {-# OVERLAPPING #-} (Serialisable XML a) => Serialisable XML (Maybe a) where
    builder _ Nothing = mempty
    builder f (Just a) = builder @XML f a

instance (Node a) => Node (Maybe a) where
    node _ Nothing = blank_element
    node qn (Just a) = node qn a
instance Node Tree where
    node qn (MkTree v l r) =
        node
            qn
            [ unode "value" $ show v
            , unode "left" l
            , unode "right" r
            ]

genSerialisable @JSON ''Tree
genSerialisable @XML ''Tree

benchmark :: Benchmark
benchmark = bgroup "tree" [jsonBenchmark, xmlBenchmark]

buildTree :: Int -> Tree
buildTree 0 = MkTree 0 Nothing Nothing
buildTree n =
    let subtree = buildTree (n - 1)
     in MkTree n (Just subtree) (Just subtree)

depths :: [Int]
depths = [1, 2, 5, 10, 15, 20]

jsonBenchmark :: Benchmark
jsonBenchmark =
    bgroup
        "json"
        [ bgroup
            "aeson"
            ( depths <&> \depth ->
                env
                    (pure $ force $ buildTree depth)
                    (bench (show depth) . nf (toStrict . encode))
            )
        , bgroup
            "serth"
            ( depths <&> \depth ->
                env
                    (pure $ force $ buildTree depth)
                    (bench (show depth) . nf (toStrict . toLazyByteString . builder @JSON []))
            )
        ]

xmlBenchmark :: Benchmark
xmlBenchmark =
    bgroup
        "xml"
        [ bgroup
            "xml"
            ( depths <&> \depth ->
                env
                    (pure $ force $ buildTree depth)
                    (bench (show depth) . nf (showElement . node blank_name))
            )
        , bgroup
            "serth"
            ( depths <&> \depth ->
                env
                    (pure $ force $ buildTree depth)
                    (bench (show depth) . nf (toStrict . toLazyByteString . builder @XML []))
            )
        ]

main :: IO ()
main =
    defaultMain
        [benchmark]
