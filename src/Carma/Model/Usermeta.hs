{-|

Proxy model for a subset of legacy usermeta model.

-}

module Carma.Model.Usermeta where

import Data.Text
import Data.Typeable
import Data.Vector

import Data.Model
import Data.Model.View

import Carma.Model.Types ()
import Carma.Model.Role hiding (ident)

data Usermeta = Usermeta
  { ident :: PK Int Usermeta          "Данные о пользователе"
  , label :: F Text                   "login" "Логин"
  , value :: F (Maybe Text)           "realName" "ФИО пользователя"
  -- TODO String-wrapped list of Role ids (to be used until usermeta
  -- is fully migrated to new models)
  , roles :: F (Maybe (Vector (IdentT Role))) "roles" "Роли в системе"
  } deriving Typeable

instance Model Usermeta where
  type TableName Usermeta = "usermetatbl"
  modelInfo = mkModelInfo Usermeta ident
  modelView _ = defaultView
