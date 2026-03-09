module Data.Serth.Base.Type (Type (..), Constructor (..), conName) where

import Language.Haskell.TH (Name)
import qualified Language.Haskell.TH as TH

-- | Our representation of a Haskell data type
data Type
    = -- | The definition of the type, with its name and constructors
      ADT Name [Constructor]
    | -- | The type is hidden behind a type variable
      TypeVariable
        -- | The name of the type variable
        Name
    | -- | The type is a prmitive
      Prim
        -- | The name of the type
        Name
    | List TH.Type

-- | A data constructor
data Constructor
    = -- | The constructor is a record-constructor: each field has a unique name
      RecordCons Name [(Name, TH.Type)]
    | -- | Non-record constructor. Fields are identifiable by their position in the list
      SimpleCons
        Name
        -- | Types of the fields, in the fields' declaration order
        [TH.Type]

conName :: Constructor -> Name
conName = \case
    RecordCons n _ -> n
    SimpleCons n _ -> n
