{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Main (main) where

import Control.DeepSeq
import Criterion.Main
import qualified Data.Aeson as Aeson
import Data.ByteString
import Data.Functor
import Data.JSON.FromJSON
import Data.JSON.Parser.Base (Error (..), Result (..), runParser)
import Data.JSON.TH (genFromJSON)
import GHC.Generics (Generic, Generic1)
import Text.Printf

data Book = Book
    { title :: String
    , releaseYear :: Integer
    , pageCount :: Integer
    , rating :: Double
    }
    deriving (Generic, Eq, Show)

data Author = Author
    { name :: String
    , age :: Int
    , books :: [Book]
    , active :: Bool
    }
    deriving (Generic, Eq, Show)

data Tree a = Tree
    { left :: Maybe (Tree a)
    , value :: a
    , right :: Maybe (Tree a)
    }
    deriving (Generic, Generic1, Eq, Show)

deriving instance (Generic a, Generic e) => Generic (Result e a)
instance (Generic a, Generic e, NFData a, NFData e) => NFData (Result e a)
deriving instance Generic Error
instance NFData Error
instance NFData Book
instance (NFData a) => NFData (Tree a)
instance NFData1 Tree
instance NFData Author
instance Aeson.FromJSON Book
instance Aeson.FromJSON Author
instance Aeson.ToJSON Book
instance Aeson.ToJSON Author
instance (Aeson.ToJSON a) => Aeson.ToJSON (Tree a)
instance (Aeson.FromJSON a) => Aeson.FromJSON (Tree a)

genFromJSON ''Book
genFromJSON ''Author
genFromJSON ''Tree

author :: Author
author = Author "Lewis Carroll" 66 [book1, book2] False

book1 :: Book
book1 = Book "Alice In Wonderland" 1865 100 0.9

book2 :: Book
book2 = Book "Alice Through the looking glass" 1871 80 0.6

main :: IO ()
main =
    defaultMain
        [ bgroup
            "object"
            [ case_ "simple" book1
            , case_ "nested" author
            ]
        , bgroup
            "list"
            $ [1, 10, 50, 100, 200, 300, 500, 700, 1000] <&> \l ->
                case_ (printf "%04d" l) (Prelude.concat $ Prelude.replicate l [book1, book2])
        , bgroup
            "tree"
            $ [1, 10, 20] <&> \l ->
                case_ (printf "%02d" l) $ buildTree l
        ]

case_ :: forall a. (NFData a, Generic a, Aeson.ToJSON a, Aeson.FromJSON a, FromJSON a) => String -> a -> Benchmark
case_ n obj =
    bgroup n $
        let
            bs = toStrict $! Aeson.encode obj
         in
            [ bench "aeson" $ nf (Aeson.decodeStrict @a) bs
            , bench "reflection" $ nf (runParser (parse @a)) bs
            ]

buildTree :: Int -> Tree Int
buildTree 0 = Tree Nothing 0 Nothing
buildTree n = Tree (Just $ buildTree (n - 1)) n (Just $ buildTree (n - 1))
