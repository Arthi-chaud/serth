{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.JSON.TH (genFromJSON) where

import Data.Bits
import Data.Functor
import Data.JSON.FromJSON
import Data.JSON.Parser.Base (Error (..))
import Data.JSON.Parser.Internal
import qualified Data.List.NonEmpty as NE
import Data.Serth.Base
import qualified FlatParse.Basic as B
import FlatParse.Stateful (switch)
import qualified FlatParse.Stateful as S
import GHC.Exts
import Language.Haskell.TH hiding (Type)
import Text.Printf (printf)

instance EDSL Exp where
    compile = return

-- | For a given data name, generates an instance for 'FromJSON' similar to this:
--
-- @
-- instance FromJSON Book where
--     {-# INLINE parse #-}
--     parse = do
--         $(char '{')
--         skipSpacesB
--         !obj <- liftS go Book{} 15#
--         skipSpacesB
--         $(char '}')
--         return obj
--       where
--         go = fieldSwitch $ do
--             !missingFields <- S.get
--             if missingFields == 0
--                 then S.ask
--                 else do
--                     skipSpacesS >> $(S.char ',') >> skipSpacesS
--                     go
--         {-# INLINE fieldSwitch #-}
--         fieldSwitch :: S.Parser Book String Book -> S.Parser Book String Book
--         fieldSwitch cont =
--             $( S.switch
--                 [|
--                     case _ of
--                         "\\"title\\"" -> do
--                             !field <- consumeCommaAndParse
--                             clearBitAndSetField 0# (\obj -> obj{title = field}) cont
--                         "\\"author\\"" -> do
--                             !field <- consumeCommaAndParse
--                             clearBitAndSetField 1# (\obj -> obj{author = field}) cont
--                     |]
--              )
-- @
genFromJSON :: Name -> Q [Dec]
genFromJSON n = do
    (tyName, tyArgNames) <- getTyConAndArgs n
    parserFunc <- genParseFunc
    let instanceTy = ConT ''FromJSON `AppT` foldl (\r a -> r `AppT` VarT a) (ConT tyName) tyArgNames
    let inlinePragma = PragmaD (InlineP 'parse Inline FunLike AllPhases)
    return
        [ InstanceD
            (Just Overlaps)
            (AppT (ConT ''FromJSON) . VarT <$> tyArgNames)
            instanceTy
            [inlinePragma, parserFunc]
        ]
  where
    genParseFunc = do
        fieldSwitchFName <- newName "fieldSwitch"
        let fieldSwitchInlineP = PragmaD (InlineP fieldSwitchFName Inline FunLike AllPhases)
        funD
            'parse
            [ clause
                []
                (normalB $ observeType n $ genParser fieldSwitchFName)
                [ return fieldSwitchInlineP
                , valD (varP fieldSwitchFName) (normalB $ observeType n genFieldSwitchExp) []
                ]
            ]

genParser :: Name -> Type -> Q Exp
genParser fieldSwitchName (ADT _ [RecordCons conN fields]) =
    [|
        let {-# INLINE go #-}
            go = $(varE fieldSwitchName) $ do
                !missingFields <- S.get
                if missingFields == 0
                    then S.ask
                    else do
                        (skipSpacesS >> $(S.char ',') >> skipSpacesS) S.<|> S.err (MissingFields $ getMissingFields missingFields)
                        go
            getMissingFields :: Int -> NE.NonEmpty String
            getMissingFields 0 = error "cannot be zero"
            getMissingFields n = NE.fromList $ go' n $(listE (litE . stringL . nameBase . fst <$> fields))
              where
                go' 0 _ = []
                go' _ [] = []
                go' idx (f : fs) = if testBit idx 0 then f : go' (idx `div` 2) fs else go' (idx `div` 2) fs
         in do
                $(B.char '{') B.<|> B.err (ExpectedValueType "object")
                skipSpacesB
                !obj <-
                    liftS
                        go
                        $(return emptyObjectExp)
                        $(litE binaryFieldCountLit)
                skipSpacesB
                $(B.char '}') B.<|> B.err (ExpectedToken '}')
                return obj
        |]
  where
    binaryFieldCountLit = IntegerL $ fromIntegral $ getBinaryFieldCount $ length fields
    emptyObjectExp = RecConE conN []
genParser _ _ = fail "Unsupported data type"

genFieldSwitchExp :: Type -> Q Exp
genFieldSwitchExp (ADT _ [RecordCons _ fields]) =
    let fieldNames = fst <$> fields
        contName = mkName "cont"
        caseBranches :: [(Name, Q Exp)]
        caseBranches =
            zip fieldNames [0 ..] <&> \(fieldName, idx) ->
                let
                    valueName = mkName "val"
                    objName = mkName "obj"
                    bitToClearLit = IntPrimL idx
                 in
                    ( fieldName
                    , [e|
                        do
                            skipSpacesS
                            $(S.char ':')
                            skipSpacesS
                            $(varP valueName) <- liftB parse
                            -- We update both the state and the int at once
                            S.ParserT
                                ( \(!fptr) (!($(varP objName))) (!addr1) (!addr2) (!missingFields) !st ->
                                    S.runParserT#
                                        $(varE contName)
                                        fptr
                                        ( $( recUpdE
                                                (varE objName)
                                                [return (fieldName, VarE valueName)]
                                           )
                                        )
                                        addr1
                                        addr2
                                        (missingFields `andI#` notI# (1# `iShiftL#` $(litE bitToClearLit)))
                                        st
                                )
                        |]
                    )
        caseExp =
            caseE (unboundVarE $ mkName "") $
                caseBranches
                    <&> \(fieldName, expr) -> match (litP $ stringL $ printf "\"%s\"" $ nameBase fieldName) (normalB expr) []
     in lamE [varP contName] (switch caseExp)
genFieldSwitchExp _ = fail "Unsupported data type"

getTyConAndArgs :: Name -> Q (Name, [Name])
getTyConAndArgs tyName = do
    info <- reify tyName
    case info of
        TyConI dec -> go dec
        e -> fail $ "Expected a type constructor. Got" ++ show e
  where
    go = \case
        DataD _ n tyVars _ _ _ -> return (n, nameFromTyVarBndr <$> tyVars)
        NewtypeD _ n tyVars _ _ _ -> return (n, nameFromTyVarBndr <$> tyVars)
        TySynD n tyVars _ -> return (n, nameFromTyVarBndr <$> tyVars)
        e -> fail $ "Expected a newtype/data/type synonym. Got " ++ show e
    nameFromTyVarBndr = \case
        PlainTV n _ -> n
        KindedTV n _ _ -> n

-- |
--
-- 1 -> 0b1
-- 2 -> 0b11
-- 3 -> 0b111
-- 4 -> 0b1111
getBinaryFieldCount :: Int -> Int
getBinaryFieldCount 0 = 0
getBinaryFieldCount n = 1 + (getBinaryFieldCount (n - 1) * 2)
