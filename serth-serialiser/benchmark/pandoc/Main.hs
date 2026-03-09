module Main (main) where

import Criterion.Main (defaultMain)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import Data.Serth.Serialiser
import Data.Serth.Serialiser.Format.JSON
import Futadata ()
import qualified Text.Pandoc.UTF8 as UTF8

import Criterion
import Text.Pandoc

main :: IO ()
main =
    defaultMain
        ( env (runIOorExplode loadAST)
            <$> [ bench "aeson" . nfAppIO (runIOorExplode . writeJSONAeson def)
                , bench "futadata" . nfAppIO (runIOorExplode . writeJSONFutadata def)
                ]
        )

loadAST :: (PandocMonad m) => m Pandoc
loadAST = do
    fileContent <- readFileStrict "benchmark/pandoc/data.md"
    readMarkdown def $ UTF8.toText fileContent

writeJSONFutadata :: (PandocMonad m) => WriterOptions -> Pandoc -> m BL.ByteString
writeJSONFutadata _ = return . BL.fromStrict . serialise @JSON

writeJSONAeson :: (PandocMonad m) => WriterOptions -> Pandoc -> m BL.ByteString
writeJSONAeson _ = return . encode

-- Note using Strict or lazy BS doesn't change anything
