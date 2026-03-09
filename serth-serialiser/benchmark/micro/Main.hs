module Main (main) where

import Criterion.Main
import qualified JSON
import qualified XML

main :: IO ()
main =
    defaultMain
        [JSON.benchmark, XML.benchmark]
