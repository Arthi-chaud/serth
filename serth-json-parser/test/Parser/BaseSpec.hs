{-# LANGUAGE OverloadedStrings #-}

module Parser.BaseSpec (spec) where

import Data.ByteString
import Data.JSON.Parser.Array
import Data.JSON.Parser.Base
import Test.Hspec

spec :: Spec
spec = do
    describe "String" $ do
        test "Simple" parseString "\"hello world\"" "hello world"
        test "Empty" parseString "\"\"" ""
        test "With hex-encoded char" parseString "\"1\\u00323\"" "123"
        test "With escaped char" parseString "\"1\\n2\"" "1\n2"
        test "With escaped slash" parseString "\"1\\\\/2\"" "1\\/2"

    describe "Integral" $ do
        let parseInteger = parseIntegral @Integer
        test "Simple" parseInteger "123" 123
        test "Zero" parseInteger "0" 0
        test "Negative" parseInteger "-1" (-1)

    describe "Double" $ do
        test "Int" parseDouble "123" 123
        test "Simple" parseDouble "1.2" 1.2
        test "Long" parseDouble "1.31411" 1.31411
        test "Negative" parseDouble "-100.1" (-100.1)

    describe "Bool" $ do
        test "True" parseBool "true" True
        test "False" parseBool "false" False

    describe "List" $ do
        let parser = parseArray $ parseIntegral @Integer
        test "Empty" parser "[]" []
        test "Singleton" parser "[0]" [0]
        test "Two elems" parser "[ 1 , -3 ]" [1, -3]
  where
    test :: (Show a, Eq a) => String -> Parser a -> ByteString -> a -> SpecWith ()
    test name parser actual expected =
        it name $ case runParser parser actual of
            (OK a _) -> a `shouldBe` expected
            e -> fail $ "Expected OK, got " ++ show e
