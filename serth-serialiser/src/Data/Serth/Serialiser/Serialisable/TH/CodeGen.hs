{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Data.Serth.Serialiser.Serialisable.TH.CodeGen (genBuilderMatch) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder (byteString)
import Data.ByteString.Builder.Internal (empty)
import Data.ByteString.Char8 (unpack)
import Data.Either
import Data.Functor
import Data.Maybe
import Data.Serth.Serialiser.FieldName (FieldName (FieldName))
import Data.Serth.Serialiser.Format
import Data.Serth.Serialiser.Serialisable (Serialisable, builder)
import Data.Serth.Serialiser.Serialisable.TH.CompiledTemplate (compileTemplate)
import Data.Serth.Serialiser.Serialisable.TH.Internal
import Data.Serth.Serialiser.Template
import Data.Serth.Base
import Language.Haskell.TH hiding (Type)

genBuilderMatch :: forall format. (Format format, Serialisable format String) => Name -> Constructor -> Template format -> Q Match
genBuilderMatch ctxVarName con template = do
    let
        fieldCount = case con of
            RecordCons _ f -> length f
            SimpleCons _ f -> length f
        -- We deconstruct the constructor, exposing its fields
        -- This function returns the name of the field based on its position
        fieldNameAtIdx i = mkName $ "field" ++ show i
        compiledTemplate =
            resolveConNameHoles @format (nameBase $ conName con) $
                compileTemplate template
        -- Retriving holes in the template, with their index
        templateHoles = lefts compiledTemplate `zip` [0 ..]
        fieldsBuildersDecls =
            templateHoles
                <&> ( \((fName, hole), idx) ->
                        ( mkName $ "builder" ++ show idx
                        , builderExpForHole @format con fName hole ctxVarName fieldNameAtIdx
                        )
                    )
        -- The 'main' expression that puts together the static portions of the template and the resolved holes
        serialisationExpr =
            let
                builderExprs =
                    compiledTemplate
                        <&> ( \case
                                Left hole ->
                                    -- From a hole, using its index in the template,
                                    -- we resolve the name of the corresponding builder
                                    let holeIdx = fromJust $ lookup hole templateHoles
                                     in varE $ fst $ fieldsBuildersDecls !! holeIdx
                                -- Note, surprisingly, having the template in a variable + using !! is slower
                                Right segment -> [|byteString $(litE $ StringL segment)|]
                            )
             in
                foldr1 (\item rest -> [|$item <> $rest|]) builderExprs
    funcBody <-
        letE
            -- let-binding for each hole's builder expression
            ((\(varName, body) -> valD (varP varName) (normalB body) []) <$> fieldsBuildersDecls)
            -- The main body
            serialisationExpr
    return $
        Match
            (ConP (conName con) [] (VarP . fieldNameAtIdx <$> [0 .. fieldCount - 1]))
            (NormalB funcBody)
            []

builderExpForHole :: forall format. (Format format) => Constructor -> FieldName -> TemplateHole -> Name -> (Int -> Name) -> Q Exp
builderExpForHole con fName@(FieldName fName') hole ctxVarName fieldNameAtIdx = case hole of
    Field fieldIdx -> [|$builderE $ctxE ($(varE $ fieldNameAtIdx fieldIdx))|]
    ConName -> [|$builderE @ByteString $ctxE $(litE . StringL . nameBase $ conName con)|]
    -- If the hole is a collection of fields
    Fields fieldsIdx ->
        let
            -- Get the array template
            (leading, inter, trailing) = arrayTemplate @format [fName]
            interBuilderVarName = mkName "inter"
            -- For each field, get builder expression
            arrayItemExprs = fieldsIdx <&> (\i -> [|$builderE $ctxE $(varE $ fieldNameAtIdx i)|])
            -- Put the fields together, intercalate-ing the 'inter' segment
            arrayBodyExpr =
                if null arrayItemExprs
                    then [|empty|]
                    else
                        foldr1
                            ( \item rest ->
                                [|$item <> $(varE interBuilderVarName) <> $rest|]
                            )
                            arrayItemExprs
         in
            [|
                let $(varP interBuilderVarName) = byteString $(litE . StringL $ unpack inter)
                 in byteString $(litE . StringL $ unpack leading)
                        <> $arrayBodyExpr
                        <> byteString $(litE . StringL $ unpack trailing)
                |]
  where
    builderE = [|builder @($(conT $ getFormatName @format))|]
    -- The context expansion expression, using the FieldName in the final serialised object
    ctxE = [|FieldName ($(litE . StringL . unpack $ fName')) : $(varE ctxVarName)|]
