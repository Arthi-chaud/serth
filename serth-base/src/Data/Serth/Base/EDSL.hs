module Data.Serth.Base.EDSL (EDSL (..)) where

import Language.Haskell.TH

-- | Typeclass to describe languages that use 'Type' values
class EDSL a where
    -- | Produces a Haskell expression from the DSL's AST
    compile :: a -> Q Exp
