module Carma.Model.Transmission where

import Data.Text
import Data.Typeable
import Data.Vector

import Data.Model
import Data.Model.View

import Carma.Model.Types()

data Transmission = Transmission
  { ident    :: PK Int Transmission "Коробка передач"
  , label    :: F Text "label" "Тип"
  , synonyms :: F (Vector Text) "synonyms" "Синонимы"
  } deriving Typeable

instance Model Transmission where
  type TableName Transmission = "Transmission"
  modelInfo = mkModelInfo Transmission ident
  modelView _ = defaultView
