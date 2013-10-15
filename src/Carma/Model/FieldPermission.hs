
module Carma.Model.FieldPermission where



import Data.Text
import Data.Model
import Data.Typeable

import Data.Model.View


data FieldPermission = FieldPermission
  { role  :: F Text "role"  "Роль"
  , model :: F Text "model" "Модель"
  , field :: F Text "field" "Внутреннее название поля"
  , r     :: F Bool "r"     "Доступно для чтения"
  , w     :: F Bool "w"     "Доступно для записи"
  } deriving Typeable


instance Model FieldPermission where
  type TableName FieldPermission = "NewCaseField"
  modelInfo = mkModelInfo FieldPermission
  modelView _ = defaultView
