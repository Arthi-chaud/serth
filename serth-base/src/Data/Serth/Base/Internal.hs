{-# LANGUAGE LambdaCase #-}

module Data.Serth.Base.Internal (varInfoToType, typeInfoToType) where

import Data.List (singleton)
import qualified Data.Serth.Base.Type as Serth
import Language.Haskell.TH

varInfoToType :: Info -> Q Serth.Type
varInfoToType = \case
    (VarI _ ty _) -> thTypeToType ty
    _ -> fail "Expected a variable name"

typeInfoToType :: Info -> Q Serth.Type
typeInfoToType = \case
    (TyConI dec) -> decToType dec
    (TyVarI n _) -> return $ Serth.TypeVariable n
    (PrimTyConI n _ _) -> return $ Serth.Prim n
    _ -> fail "Expected a type constructor name"

thTypeToType :: Type -> Q Serth.Type
thTypeToType = \case
    (ConT ty) -> reify ty >>= typeInfoToType
    (AppT (ConT ty) _) -> reify ty >>= typeInfoToType
    (AppT ListT ty) -> return $ Serth.List ty
    (AppT ty _) -> thTypeToType ty
    (VarT n) -> return $ Serth.TypeVariable n
    ty -> fail $ "Don't know how to handle this type: " ++ show ty

decToType :: Dec -> Q Serth.Type
decToType = \case
    (NewtypeD _ n _ _ con _) -> Serth.ADT n . singleton <$> thConToConstructor con
    (DataD _ n _ _ cons _) -> Serth.ADT n <$> mapM thConToConstructor cons
    (TySynD _ _ ty) -> thTypeToType ty
    f -> fail $ "Expected a data/newtype declaration: " ++ show f

thConToConstructor :: (MonadFail m) => Con -> m Serth.Constructor
thConToConstructor = \case
    (NormalC n bts) -> return $ Serth.SimpleCons n (snd <$> bts)
    (RecC n vbt) -> return $ Serth.RecordCons n $ (\(fName, _, ty) -> (fName, ty)) <$> vbt
    (InfixC (_, t1) n (_, t2)) -> return $ Serth.SimpleCons n [t1, t2]
    (ForallC _ _ con) -> thConToConstructor con
    con -> fail $ "Don't know how to handle this constructor: " ++ show con
