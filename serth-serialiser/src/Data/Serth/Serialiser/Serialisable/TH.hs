{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Serth.Serialiser.Serialisable.TH (
    genSerialisable,
    genSerialisables,
    genSerialisableWithOptions,
    genSerialisablesWithOptions,
    module Data.Serth.Serialiser.Serialisable.TH.Options,
) where

import Data.Serth.Serialiser.Format (Format)
import Data.Serth.Serialiser.Serialisable (Serialisable (builder))
import Data.Serth.Serialiser.Serialisable.TH.CodeGen (genBuilderMatch)
import Data.Serth.Serialiser.Serialisable.TH.Internal
import Data.Serth.Serialiser.Serialisable.TH.Options
import Data.Serth.Serialiser.Template (Template)
import Data.Serth.Base
import Language.Haskell.TH hiding (Prim)

data SerialisationExpr format
    = MkExpr
        Name -- Name of the context variable
        Name -- Name of the value to serialise
        [(Constructor, Template format)]

instance (Format format, Serialisable format String) => EDSL (SerialisationExpr format) where
    compile (MkExpr ctxtName varName conTempl) = caseE (varE varName) (uncurry (genBuilderMatch ctxtName) <$> conTempl)

genSerialisable :: forall format. (Format format, Serialisable format String) => Name -> Q [Dec]
genSerialisable n = genSerialisableWithOptions @format n defaultOptions

genSerialisableWithOptions :: forall format. (Format format, Serialisable format String) => Name -> Options -> Q [Dec]
genSerialisableWithOptions tyName options = do
    let ctxName = mkName "ctx"
        valName = mkName "val"
        formatName = getFormatName @format
    caseExpr <-
        observeType
            tyName
            ( \case
                ADT _ cons ->
                    let
                        consSiblings = length cons - 1
                     in
                        return $ MkExpr ctxName valName $ (\con -> (con, templateFromConstructor @format options consSiblings con)) <$> cons
                TypeVariable n -> fail $ "Cannot derive template from type variable " ++ nameBase n
                Prim n -> fail $ "Cannot derive template from primitive " ++ nameBase n
                List _ -> fail "Cannot derive template from lists"
            )
    func <- funD 'builder [clause [varP ctxName, varP valName] (return $ NormalB caseExpr) []]
    instanceType <- [t|Serialisable $(conT formatName) $(conT tyName)|]
    return [InstanceD (Just Overlapping) [] instanceType [func]]

genSerialisables :: forall format. (Format format, Serialisable format String) => [Name] -> Q [Dec]
genSerialisables tyNames = genSerialisablesWithOptions @format tyNames defaultOptions

genSerialisablesWithOptions :: forall format. (Format format, Serialisable format String) => [Name] -> Options -> Q [Dec]
genSerialisablesWithOptions tyNames options = concat <$> (flip (genSerialisableWithOptions @format) options `mapM` tyNames)
