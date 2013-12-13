module Carma.Model.Engine where

import Data.Text
import Data.Typeable
import Data.Vector

import Data.Model
import Data.Model.View

import Carma.Model.Types()

data Engine = Engine
  { ident    :: PK Int Engine   "Тип двигателя"
  , label    :: F Text          "label" "Тип"
  , synonyms :: F (Maybe (Vector Text)) "synonyms" "Синонимы"
  } deriving Typeable

instance Model Engine where
  type TableName Engine = "Engine"
  modelInfo = mkModelInfo Engine ident
  modelView _ = defaultView
