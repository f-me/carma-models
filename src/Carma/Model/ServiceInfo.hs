
module Carma.Model.ServiceInfo where


import Data.Text (Text)
import Data.Typeable

import Carma.Model.Types()

import Data.Model
import Data.Model.View

import Carma.Model.OldProgram (OldProgram)
import Carma.Model.ServiceNames (ServiceNames)

data ServiceInfo = ServiceInfo
  {ident   :: PK Int ServiceInfo ""
  ,program :: F (IdentI OldProgram)   "program" "Программа"
  ,service :: F (IdentI ServiceNames) "service" "Услуга"
  ,info    :: F Text                  "info"    "Условия"
  } deriving Typeable

instance Model ServiceInfo where
  type TableName ServiceInfo = "ServiceInfo"
  modelInfo = mkModelInfo ServiceInfo ident
  modelView _ = modifyView defaultView
    [readonly program, readonly service, textarea info]

