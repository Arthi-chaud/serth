{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module THSpec (spec) where

import Data.JSON.FromJSON (FromJSON (..))
import Data.JSON.Parser.Base (Error (..))
import Data.JSON.TH (genFromJSON)
import Data.List.NonEmpty
import FlatParse.Basic hiding (Parser)
import Test.Hspec

data MyObject = MyObject {a :: Int, b :: String} deriving (Eq, Show)

genFromJSON ''MyObject

spec :: Spec
spec =
    describe "Generated Parser" $ do
        it "should parse object" $ do
            case runParser parse "{\"a\": 1, \"b\": \"Hello World\"}" of
                OK (MyObject a' b') _ -> do
                    a' `shouldBe` 1
                    b' `shouldBe` "Hello World"
                e -> fail $ "Expected OK, got " ++ show e
        describe "Error handling" $ do
            it "empty object" $ do
                case runParser (parse @MyObject) "{ \"a\": 1 }" of
                    (Err (MissingFields fs)) -> fs `shouldBe` ("b" :| [])
                    e -> fail $ "Expected a missing field error, got " ++ show e
            it "missing field" $ do
                case runParser (parse @MyObject) "{ \"a\": 1 }" of
                    (Err (MissingFields _)) -> 'a' `shouldBe` 'a'
                    e -> fail $ "Expected a missing field error, got " ++ show e
            it "bad type" $ do
                case runParser (parse @MyObject) "123" of
                    (Err er) -> er `shouldBe` ExpectedValueType "object"
                    e -> fail $ "Expected an error, got " ++ show e
            it "bad field type" $ do
                case runParser (parse @MyObject) "{ \"a\": \"1\" }" of
                    (Err er) -> er `shouldBe` ExpectedValueType "integer"
                    e -> fail $ "Expected an error, got " ++ show e
