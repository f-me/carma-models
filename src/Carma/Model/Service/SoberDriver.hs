module Carma.Model.Service.SoberDriver where

import Data.Text
import Data.Typeable
import Data.Aeson((.=), object)
import Data.Scientific

import Data.Model
import Data.Model.View
import Carma.Model.LegacyTypes
import Carma.Model.Service (Service)


data SoberDriver = SoberDriver
  { ident :: PK Int SoberDriver ""
  , fromAddress_address :: F PickerField "fromAddress_address" "Откуда везти"
  , fromAddress_comment :: F (Maybe Text)"fromAddress_comment" "Примечания"
  , fromAddress_coords  :: F PickerField "fromAddress_coords" "Координаты"
  , fromAddress_map     :: F MapField    "fromAddress_map" ""

  , toAddress_address :: F PickerField "toAddress_address" "Куда везти"
  , toAddress_comment :: F (Maybe Text)"toAddress_comment" "Примечания"
  , toAddress_coords  :: F PickerField "toAddress_coords" "Координаты"
  , toAddress_map     :: F MapField    "toAddress_map" ""

  , isCountryRide       :: F Bool "isCountryRide" "За городом"
  , suburbanMilage      :: F (Maybe Scientific) "suburbanMilage" "Пробег за городом"
  , totalMilage         :: F (Maybe Scientific) "totalMilage" "Километраж по тахометру"
  , partnerWarnedInTime :: F (Maybe Bool) "partnerWarnedInTime" "Партнёр предупредил вовремя"
  }
  deriving Typeable


instance Model SoberDriver where
  type TableName SoberDriver = "sobertbl"
  type Parent SoberDriver = Service
  parentInfo = ExParent modelInfo
  modelInfo = mkModelInfo SoberDriver ident
  modelView v = case parentView v :: Maybe (ModelView SoberDriver) of
    Nothing -> Nothing
    Just mv -> Just
      $ modifyView (mv {mv_title = "Трезвый водитель"})
      $ setMeta "visibleIf" (object ["isCountryRide" .= [True]]) suburbanMilage
      : setMeta "visibleIf" (object ["isCountryRide" .= [True]]) totalMilage
      : setMeta "visibleIf" (object ["isCountryRide" .= [True]]) partnerWarnedInTime
      : widget "partnerWarnedInTime-btn" partnerWarnedInTime
      : mapWidget fromAddress_address fromAddress_coords fromAddress_map
      ++ mapWidget toAddress_address toAddress_coords toAddress_map
