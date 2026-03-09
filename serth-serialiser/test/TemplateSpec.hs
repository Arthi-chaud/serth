{-# LANGUAGE OverloadedStrings #-}

module TemplateSpec (spec) where

import Data.Serth.Serialiser.FieldName (FieldName (FieldName))
import Data.Serth.Serialiser.Format.JSON
import Data.Serth.Serialiser.Serialisable.TH.CompiledTemplate
import Data.Serth.Serialiser.Template
import Test.Hspec

spec :: Spec
spec = do
    describe "Compile template" $ do
        it_
            "should merge chars"
            (Token 'a' <> Token 'b' <> Token 'c')
            [Right "abc"]

        it_
            "should insert hole (leading)"
            (hole <> Token 'b')
            [Left (FieldName "", Field 1), Right "b"]

        it_
            "should insert hole (middle)"
            (Token 'a' <> Token 'b' <> hole <> Token 'c')
            [Right "ab", Left (FieldName "", Field 1), Right "c"]

        it_
            "should merge char and bytestring"
            (Token 'a' <> Tokens "bc")
            [Right "abc"]

        it_
            "should merge bytestring and char"
            (Tokens "ab" <> Token 'c')
            [Right "abc"]

        it_
            "should return empty list"
            Empty
            []
        it_
            "with real JSON"
            (Token '{' <> Tokens "\"a\":" <> mkHole 1 <> Tokens ",\"b\":" <> mkHole 2 <> Token '}')
            [Right "{\"a\":", Left (FieldName "", Field 1), Right ",\"b\":", Left (FieldName "", Field 2), Right "}"]
  where
    hole = (Hole (FieldName "") $ Field 1) :: Template JSON
    mkHole n = Hole (FieldName "") (Field n)
    it_ tName template expected = it tName $ compileTemplate (template :: Template JSON) `shouldBe` expected
