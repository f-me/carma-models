
module Carma.Model.NewCaseField where

import Data.Text
import Data.Typeable
import Data.Model
import Data.Model.View
import Carma.Model.Types()
import Carma.Model.Program (Program)


data NewCaseField = NewCaseField
  {ident    :: PK Int NewCaseField ""
  ,field    :: F Text             "field"    "Внутреннее название поля"
  ,program  :: F (IdentI Program) "program"  "Название программы"
  ,label    :: F Text             "label"    "Подпись к полю"
  ,info     :: F Text             "info"     "Текст для всплывающей подсказки"
  ,required :: F Bool             "required" "Обязательное поле"
  ,r        :: F Bool             "r"        "Доступно для чтения"
  ,w        :: F Bool             "w"        "Доступно для записи"
  }
  deriving Typeable


instance Model NewCaseField where
  type TableName NewCaseField = "NewCaseField"
  modelInfo = mkModelInfo NewCaseField ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
