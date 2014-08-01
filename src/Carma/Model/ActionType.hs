{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.ActionType where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Data.Model.TH

import Carma.Model.Types (TInt)
import Carma.Model.PgTypes()

data ActionType = ActionType
  { ident
    :: PK Int ActionType "Действие"
  , label
    :: F Text "label" "Тип действия"
  , desc
    :: F Text "desc" "Описание"
  , priority
    :: F TInt "priority" "Приоритет"
  } deriving Typeable

mkIdents [t|ActionType|]
 [ ("orderService", 1)
 , ("tellClient", 3)
 , ("checkStatus", 4)
 , ("checkEndOfService", 6)
 , ("closeCase", 7)
 , ("cancelService", 9)
 , ("tellMeMore", 19)
 ]

instance Model ActionType where
  type TableName ActionType = "ActionType"
  idents = Carma.Model.ActionType.idents
  modelInfo = mkModelInfo ActionType ident
  modelView = \case
    "" -> Just $ modifyView defaultView $
          [ infoText "actpriority" priority
          ]
    _  -> Nothing
