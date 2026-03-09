{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module JSONSpec (spec) where

import Data
import Data.Aeson
import Data.ByteString (toStrict)
import Data.Serth.Serialiser
import Data.Serth.Serialiser.Format.JSON
import Test.Hspec

instance FromJSON MySimpleRecord
instance FromJSON MyArrayRecord
instance FromJSON MySuperRecord
instance FromJSON MySuperArrayRecord
instance ToJSON MyEither where
    toEncoding = genericToEncoding $ defaultOptions{sumEncoding = defaultTaggedObject}

spec :: Spec
spec = do
    describe "Serialise records" $ do
        test "Simple record" $ MSR 1
        test "Record with array" $ MAR $ replicate 1000 10
        test "Record with nested record" $ MSPR (-1) $ MAR [1, 2, 3]
        test "Record with list of nested record" $ MSPAR (replicate 10 (MSPR (-1) $ MAR [1, 2, 3]))

        testBytes "Non-record ADT, zero fields" MyNull
        testBytes "Non-record ADT, one field" $ MyLeft 10
        testBytes "Non-record ADT, two fields" $ MyRight "1" 2
  where
    test tName obj =
        it tName $
            let
                serialised = serialise @JSON obj
                unserialised = decodeStrict serialised
             in
                unserialised `shouldBe` Just obj
    testBytes tName obj = it tName $ serialise @JSON obj `shouldBe` toStrict (encode obj)
