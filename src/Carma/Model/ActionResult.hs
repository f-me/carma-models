{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.ActionResult where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Data.Model.TH

import Carma.Model.Types()
import Carma.Model.PgTypes()

data ActionResult = ActionResult
  { ident
    :: PK Int ActionResult "Результат действия"
  , label
    :: F Text "label" "Название результата"
  } deriving Typeable

mkIdents [t|ActionResult|]
 [ ("serviceOrdered", 1)
 , ("serviceOrderedSMS", 2)
 , ("clientCanceledService", 4)
 , ("defer", 6)
 , ("serviceInProgress", 8)
 , ("serviceDone", 10)
 , ("caseClosed", 11)
 , ("clientComplained", 1030)
 , ("closeService", 1040)
 , ("couldNotReach", 1050)
 , ("falseCall", 1060)
 ]

instance Model ActionResult where
  type TableName ActionResult = "ActionResult"
  idents = Carma.Model.ActionResult.idents
  modelInfo = mkModelInfo ActionResult ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
