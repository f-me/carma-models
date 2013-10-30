module Carma.Model.Case.Type where

import Data.Text
import Data.Typeable

import Data.Time.Clock    (UTCTime)

import Data.Model

import Carma.Model.Program  (Program)
import Carma.Model.CarMake  (CarMake)
import Carma.Model.CarModel (CarModel)
import Carma.Model.Wazzup   (Wazzup)
import Carma.Model.City     (City)


data Case = Case
  { callDate
    :: F UTCTime "callDate" "Дата звонка" -- FIXME: it's UTCTime actually
  , vwcreatedate
    :: F UTCTime "vwcreatedate" "Дата звонка"
  , callTaker
    :: F Text "callTaker" "Сотрудник РАМК"
  , comment
    :: F (Ident Wazzup) "comment" "Что случилось"
    -- , diagnosis1
    --   :: F (Ident Diagnosis1) "diagnosis1" "Система"
    -- , diagnosis2
    --   :: F (Ident Diagnosis2) "diagnosis2" "Узел/деталь"
    -- , diagnosis3
    --   :: F (Ident Diagnosis3) "diagnosis3" "Описание причины неисправности"
    -- , diagnosis4
    --   :: F (Ident Diagnosis4) "diagnosis4" "Рекомендация"
  , contact_name
    :: F (Maybe Text) "contact_name" "Звонящий"
  , contact_phone1
    :: F (Maybe Text) "contact_phone1" "Контактные телефоны"
  , contact_phone2
    :: F (Maybe Text) "contact_phone2" ""
  , contact_phone3
    :: F (Maybe Text) "contact_phone3" ""
  , contact_phone4
    :: F (Maybe Text) "contact_phone4" ""
  , contact_email
    :: F (Maybe Text) "contact_email" "Email"
  , contact_contactOwner
    :: F Bool "contact_contactOwner" "Звонящий владелец?"
  , contact_ownerName
    :: F (Maybe Text) "contact_ownerName" "Владелец"
  , contact_ownerPhone1
    :: F (Maybe Text) "contact_ownerPhone1" "Контактные телефоны"
  , contact_ownerPhone2
    :: F (Maybe Text) "contact_ownerPhone2" ""
  , contact_ownerPhone3
    :: F (Maybe Text) "contact_ownerPhone3" ""
  , contact_ownerPhone4
    :: F (Maybe Text) "contact_ownerPhone4" ""
  , contact_ownerEmail
    :: F (Maybe Text) "contact_ownerEmail" "Email"
  , program
    :: F (Ident Program) "program" "Программа"
  , car_vin
    :: F (Maybe Text) "car_vin" "VIN"
  , car_make
    :: F (Ident CarMake) "car_make" "Марка"
  , car_model
    :: F (Ident CarModel) "car_model" "Модель"
    --  , car_seller
    --    :: F (Maybe Text) "car_seller" "Дилер, продавший автомобиль"
  , car_plateNum
    :: F (Maybe Text) "car_plateNum" "Госномер"
    --  , car_makeYear
    --    :: F (Maybe Text) "car_makeYear" "Год производства автомобиля"
    -- , car_color
    --   :: F (Ident Colors) "car_color" "Цвет"
  , car_buyDate
    :: F UTCTime "car_buyDate" "Дата покупки"
  , car_checkupDate
    :: F UTCTime "car_checkupDate" "Дата последнего ТО"
    --  , car_dealerTO
    --    :: F (Maybe Text) "car_dealerTO" "Дилер у которого проходило последнее ТО"
    --  , car_mileage
    --    :: F (Maybe Text) "car_mileage" "Текущий пробег"
    --  , car_checkupMileage
    --    :: F (Maybe Text) "car_checkupMileage" "Пробег на последнем ТО"
  , car_warrantyStart
    :: F UTCTime "car_warrantyStart" "Дата начала действия программы"
  , car_warrantyEnd
    :: F UTCTime "car_warrantyEnd" "Дата окончания действия программы"
    -- , car_contractType
    --   :: F (Ident ContractType) "car_contractType" "Тип контракта"
    -- , car_transmission
    --   :: F (Ident Transmission) "car_transmission" "Коробка передач"
    -- , car_engine
    --   :: F (Ident EngineType) "car_engine" "Тип двигателя"
    --  , car_liters
    --    :: F (Maybe Text) "car_liters" "Объём двигателя"
    --  , car_capacity
    --    :: F (Maybe Text) "car_capacity" "Вместимость"
    --  , car_dims
    --    :: F (Maybe Text) "car_dims" "Габариты"
    --  , car_weight
    --    :: F (Maybe Text) "car_weight" "Масса"
    --  , car_checkPeriod
    --    :: F (Maybe Text) "car_checkPeriod" "Межсервисный интервал"
    -- , car_class
    --   :: F (Ident CarClasses) "car_class" "Класс автомобиля"
    --  , car_makeCode
    --    :: F (Maybe Text) "car_makeCode" "Код марки автомобиля"
    --  , car_modelCode
    --    :: F (Maybe Text) "car_modelCode" "Код модели автомобиля"
    --  , car_faultCode
    --    :: F (Maybe Text) "car_faultCode" "Код неиправности автомобиля"
  , cardNumber_cardNumber
    :: F (Maybe Text) "cardNumber_cardNumber" "Номер карты участника"
  , cardNumber_validFrom
    :: F UTCTime "cardNumber_validFrom" "Дата регистрации в программе"
  , cardNumber_validUntil
    :: F UTCTime "cardNumber_validUntil" "Программа действует до (дата)"
    --  , cardNumber_validUntilMilage
    --    :: F (Maybe Text) "cardNumber_validUntilMilage" "Программа действует до (пробег)"
    --  , cardNumber_milageTO
    --    :: F (Maybe Text) "cardNumber_milageTO" "Пробег при регистрации в программе"
    --  , cardNumber_serviceInterval
    --    :: F (Maybe Text) "cardNumber_serviceInterval" "Межсервисный интервал"
    --  , cardNumber_cardOwner
    --    :: F (Maybe Text) "cardNumber_cardOwner" "ФИО владельца карты"
    --  , cardNumber_manager
    --    :: F (Maybe Text) "cardNumber_manager" "ФИО менеджера"
    -- , vinChecked
    --   :: F (Ident VINChecked) "vinChecked" "Участие в программе"
  , city
    :: F (Ident City) "city" "Город"
  , caseAddress_address
    :: F (Maybe Text) "caseAddress_address" "Адрес"
    --  , caseAddress_comment
    --    :: F (Maybe Text) "caseAddress_comment" "Примечания"
    --  , caseAddress_coords
    --    :: F (Maybe Text) "caseAddress_coords" "Координаты"
    --  , caseAddress_map
    --    :: F (Maybe Text) "caseAddress_map" ""
    --  , temperature
    --    :: F (Maybe Text) "temperature" "Температура"
  , repair
    :: F UTCTime "repair" "Дата починки"
    --  , accord
    --    :: F (Maybe Text) "accord" "Номер согласования"
    --  , dealerCause
    --    :: F (Maybe Text) "dealerCause" "Неисправность со слов дилера/партнёра"
    -- , caseStatus
    --   :: F (Ident CaseStatuses) "caseStatus" "Статус кейса"
  , psaExportNeeded
    :: F Bool "psaExportNeeded" "Требуется выгрузка в PSA"
  , psaExported
    :: F Bool "psaExported" "Выгружен в PSA"
    --  , claim
    --    :: F Text "claim" "Претензия / Благодарность"
    --  , services
    --    :: F Text "services" "Услуги"
    --  , actions
    --    :: F Text "actions" "Действия"
    --  , comments
    --    :: F Text "comments" ""
    --  , files
    --    :: F Text "files" "Прикрепленные файлы"
  } deriving Typeable
