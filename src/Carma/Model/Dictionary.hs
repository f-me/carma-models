

module Carma.Model.Dictionary where

import Data.Text
import Data.Model
import Data.Typeable
import Data.Vector (Vector)

import Data.Model.View
import Carma.Model.Types()
import Carma.Model.PgTypes()

data Dictionary = Dictionary
  {ident
    :: PK Int Dictionary ""
  ,name
    :: F Text                         "name"        "Название"
  ,description
    :: F Text                         "description" "Описание"
  ,parent
    :: F (Vector (IdentI Dictionary)) "parent"      "Родительский словарь"
  ,majorFields
    :: F (Vector Text)                "majorFields" "Важные поля"
  }
  deriving Typeable


instance Model Dictionary where
  type TableName Dictionary = "Dictionary"
  modelInfo = mkModelInfo Dictionary ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
