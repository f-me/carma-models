
module Carma.Model.Service.Rent where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Carma.Model.LegacyTypes
import Carma.Model.Service (Service)


data Rent = Rent
  { ident :: PK Int Rent
  , towDealer_partner   :: F Text "towDealer_partner" "Дилер"
  , towDealer_partnerId :: F Text "towDealer_partnerId" ""
  , towDealer_address   :: F Text "towDealer_address" "Адрес"
  , towDealer_coords    :: F Text "towDealer_coords" "Координаты"
  , rentAddress_address :: F PickerField "rentAddress_address" "Адрес доставки"
  , rentAddress_comment :: F Text "rentAddress_comment" "Примечания"
  , rentAddress_coords  :: F PickerField "rentAddress_coords" "Координаты"
  , rentAddress_map     :: F MapField "rentAddress_map" ""
  , vinRent             :: F Text "vinRent" "VIN подменного автомобиля"
  , carClass            :: F (IdentT CarClass) "carClass" "Класс автомобиля"
  , providedFor         :: F Text "providedFor"
                           "Срок, на который предоставлен автомобиль (дней)"
  , rentedMake          :: F (IdentT CarMakers) "rentedMake"
                           "Марка предоставленного автомобиля"
  , rentedModel         :: F (IdentT CarModels) "rentedModel"
                           "Модель предоставленного автомобиля"
  }
  deriving Typeable


instance Model Rent where
  type TableName Rent = "renttbl"
  type Parent Rent = Service
  modelInfo = mkModelInfo Rent ident
  modelView _ = (defaultView :: ModelView Rent)
    {mv_title = "Подменный автомобиль"}
