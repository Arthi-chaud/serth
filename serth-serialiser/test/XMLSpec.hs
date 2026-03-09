{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module XMLSpec (spec) where

import Data
import Data.Serth.Serialiser
import Data.Serth.Serialiser.Format.XML
import Test.Hspec

spec :: Spec
spec = describe "Serialise records" $ do
    test "Simple record" (MSR 1) "<a>1</a>"
    test
        "Record with array"
        (MAR $ replicate 2 10)
        "<array>10</array><array>10</array>"
    test
        "Record with nested record"
        (MSPR (-1) $ MAR [1, 2])
        "<number>-1</number><nested><array>1</array><array>2</array></nested>"

    test
        "Non-Record with 0 fields"
        MyNull
        "<tag>MyNull</tag>"
    test
        "Non-Record with 1 field"
        (MyLeft (1 :: Int))
        "<tag>MyLeft</tag><contents>1</contents>"
    test
        "Non-Record with 2 fields"
        (MyRight "1" (2 :: Int))
        "<tag>MyRight</tag><contents>1</contents><contents>2</contents>"
  where
    test tName obj expected =
        it tName $
            let
                serialised = serialise @XML obj
             in
                serialised `shouldBe` expected
