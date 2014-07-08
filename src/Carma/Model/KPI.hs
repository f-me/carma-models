module Carma.Model.KPI where

import           Data.Typeable

import           Data.Time.Clock (DiffTime)

import           Data.Model
import           Data.Model.View

import           Carma.Model.Usermeta (Usermeta)
import           Carma.Model.Types (UserStateVal)


data FrontKPI = FrontKPI
  { ident        :: PK Int FrontKPI     "KPI пользователя"
  , user         :: F (IdentI Usermeta) "user"          "Оператор"
  , currentState :: F UserStateVal      "currentState"  "Текущий статус"

  , inCurrent    :: F DiffTime "inCurrent" "Время"
  , inReady      :: F DiffTime "inReady"   "Готов"
  , inBusy       :: F DiffTime "inBusy"    "Занят"
  , inDinner     :: F DiffTime "inDinner"  "Обед"
  , inRest       :: F DiffTime "inRest"    "Перерыв"
  , inServiceBreak
                 :: F DiffTime "inServiceBreak" "Служебный перерыв"
  , allRest      :: F DiffTime "inRest"         "Всего в перерывах"
  , loggedIn     :: F DiffTime "inRest"         "Всего в системе"

  , infoCall     :: F DiffTime "infoCallTime"  "В разговоре: информационные"
  , infoCount    :: F Int     "infoCallCount" "Количество: информационные"

  , caseCall     :: F DiffTime "infoCallTime"  "В разговоре: Обработка кейса"
  , caseCount    :: F Int     "infoCallCount" "Количество: Обработка кейса"

  , newCall      :: F DiffTime "infoCallTime"  "В разговоре: Создание кейса"
  , newCount     :: F Int     "infoCallCount" "Количество: Создание кейса"

  , totalCall  :: F DiffTime   "totalCall"  "Итого: Время в разговоре"
  , totalCount :: F Int       "totalCount" "Итого: Количество звонков"
  } deriving (Typeable)

instance Model FrontKPI where
  type TableName FrontKPI = "FrontKPI"
  modelInfo = mkModelInfo FrontKPI ident
  modelView = \case
    "" -> Just $ modifyView (defaultView) [invisible ident]
    _  -> Nothing