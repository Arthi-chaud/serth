{-# LANGUAGE OverloadedStrings #-}

module Parser.ErrorSpec (spec) where

import Data.ByteString
import Data.JSON.FromJSON
import Data.JSON.Parser.Base
import Test.Hspec
import Text.Printf (printf)

spec :: Spec
spec = do
    describe "Wrong Value type" $ do
        test @String "expected string" "123" (ExpectedValueType "string")
        test @Int "expected number" "\"123\"" (ExpectedValueType "integer")
        test @Bool "expected boolean" "tru" (ExpectedValueType "boolean")
        test @[Int] "expected list" "1" (ExpectedValueType "list")
    describe "Invalid JSON" $ do
        test @Double "missing decimal part" "123." MissingDecimalPart
  where
    test :: forall a. (Show a, FromJSON a) => String -> ByteString -> Error -> SpecWith ()
    test name bs expected =
        it name $ case runParser (parse @a) bs of
            (Err e) -> e `shouldBe` expected
            e -> fail $ printf "Expected %s, got %s" (show expected) (show e)
