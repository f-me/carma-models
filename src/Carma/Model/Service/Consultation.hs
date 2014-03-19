
module Carma.Model.Service.Consultation where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Carma.Model.LegacyTypes
import Carma.Model.Service (Service)


data Consultation = Consultation
  { ident      :: PK Int Consultation ""
  , consType   :: F (IdentT ConsultationType) "consType" "Тип консультации"
  , whatToSay1 :: F Text "whatToSay1" "Описание проблемы"
  }
  deriving Typeable

instance Model Consultation where
  type TableName Consultation = "consultationtbl"
  type Parent Consultation = Service
  modelInfo = mkModelInfo Consultation ident
  modelView _ = Just $ (defaultView :: ModelView Consultation)
    {mv_title = "Консультация"}
