
module Carma.Model.Service.DeliverCar where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Carma.Model.LegacyTypes
import Carma.Model.Service (Service)


data DeliverCar = DeliverCar
  { ident :: PK Int DeliverCar
  , toAddress_address :: F PickerField "toAddress_address" "Куда везти"
  , toAddress_comment :: F Text        "toAddress_comment" "Примечания"
  , toAddress_coords  :: F PickerField "toAddress_coords"  "Координаты"
  , toAddress_map     :: F MapField    "toAddress_map"     ""
  }
  deriving Typeable

instance Model DeliverCar where
  type TableName DeliverCar = "delivercartbl"
  type Parent DeliverCar = Service
  modelInfo = mkModelInfo DeliverCar ident
  modelView _ = (defaultView :: ModelView DeliverCar)
    {mv_title = "Доставка ТС"}
