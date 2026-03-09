{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib where

import qualified Data.Aeson as A
import Data.JSON.TH (genFromJSON)
import Data.Serth.Serialiser.Format.JSON
import Data.Serth.Serialiser.Serialisable.TH (genSerialisables)
import GHC.Generics

data User = User {id :: Int, name :: String, email :: String, enabled :: Bool}
data UserDTO = UserDTO {name_ :: String, email_ :: String, enabled_ :: Bool}

deriving instance Generic User
deriving instance Generic UserDTO

instance A.FromJSON User
instance A.FromJSON UserDTO
instance A.ToJSON User
instance A.ToJSON UserDTO

genFromJSON ''UserDTO
genFromJSON ''User
genSerialisables @JSON [''User, ''UserDTO]
