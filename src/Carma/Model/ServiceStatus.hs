{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.ServiceStatus where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Data.Model.TH

import Carma.Model.Types()
import Carma.Model.PgTypes()

data ServiceStatus = ServiceStatus
  { ident
    :: PK Int ServiceStatus "Статус кейса"
  , label
    :: F Text "label" "Тип"
  } deriving Typeable

mkIdents [t|ServiceStatus|]
 [ ("creating", 2)
 , ("clientCanceled", 4)
 , ("mistake", 9)
 , ("canceled", 14)
 , ("ordered", 15)
 , ("delayed", 16)
 , ("inProgress", 17)
 , ("falseCall", 18)
 , ("ok", 19)
 , ("closed", 20)
 ]

instance Model ServiceStatus where
  type TableName ServiceStatus = "ServiceStatus"
  idents = Carma.Model.ServiceStatus.idents
  modelInfo = mkModelInfo ServiceStatus ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing