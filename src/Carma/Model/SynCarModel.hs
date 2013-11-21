module Carma.Model.SynCarModel where

import Data.Text
import Data.Typeable
import Data.Model
import Data.Model.View
import Carma.Model.Types()

import Carma.Model.CarMake  (CarMake)
import Carma.Model.CarModel (CarModel)

data SynCarModel = SynCarModel
  { ident   :: PK Int SynCarModel
  , make    :: F (IdentI CarMake)   "make"  "Марка"
  , model   :: F (IdentI CarModel)  "model" "Модель"
  , synonym :: F Text               "label" "Синоним"
  }
  deriving Typeable


instance Model SynCarModel where
  type TableName SynCarModel = "SynCarModel"
  modelInfo = mkModelInfo SynCarModel ident
  modelView _ = defaultView
