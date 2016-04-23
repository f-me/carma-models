{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.ClientRefusalReason where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Data.Model.TH

import Carma.Model.Types()
import Carma.Model.PgTypes()

data ClientRefusalReason = ClientRefusalReason
  { ident
    :: PK Int ClientRefusalReason "Причина отказа клиента"
  , label
    :: F Text "label" "Причина"
  } deriving Typeable

mkIdents [t|ClientRefusalReason|]
 [ ("partnerDelay", 43)
 ]

instance Model ClientRefusalReason where
  type TableName ClientRefusalReason = "ClientRefusalReason"
  modelInfo = mkModelInfo ClientRefusalReason ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
