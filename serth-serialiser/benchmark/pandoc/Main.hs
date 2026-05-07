module Main (main) where

import Criterion
import Criterion.Main (defaultMain)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import Data.Serth.Serialiser
import Data.Serth.Serialiser.Format.JSON
import Data.Serth.Serialiser.Format.XML
import Futadata ()
import Text.Pandoc
import qualified Text.Pandoc.UTF8 as UTF8

main :: IO ()
main = do
    defaultMain
        [ env (runIOorExplode loadAST) $ \ast ->
            bgroup
                "json"
                [ bench "aeson" $ nfAppIO (runIOorExplode . writeJSONAeson def) ast
                , bench "serth" $ nfAppIO (runIOorExplode . writeJSONFutadata def) ast
                ]
        , env (runIOorExplode loadAST) $ \ast ->
            bgroup
                "xml"
                [ bench "xml" $ nfAppIO (runIOorExplode . writeXML def) ast
                , bench "serth" $ nfAppIO (runIOorExplode . writeXMLFutadata def) ast
                ]
        ]

loadAST :: (PandocMonad m) => m Pandoc
loadAST = do
    fileContent <- readFileStrict "benchmark/pandoc/data.md"
    readMarkdown def $ UTF8.toText fileContent

writeJSONFutadata :: (PandocMonad m) => WriterOptions -> Pandoc -> m BL.ByteString
writeJSONFutadata _ = return . BL.fromStrict . serialise @JSON

writeXMLFutadata :: (PandocMonad m) => WriterOptions -> Pandoc -> m BL.ByteString
writeXMLFutadata _ = return . BL.fromStrict . serialise @XML

writeJSONAeson :: (PandocMonad m) => WriterOptions -> Pandoc -> m BL.ByteString
writeJSONAeson _ = return . encode

writeXML :: (PandocMonad m) => WriterOptions -> Pandoc -> m BL.ByteString
writeXML _ = return . encode

-- Note using Strict or lazy BS doesn't change anything
