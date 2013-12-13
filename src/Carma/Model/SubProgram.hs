{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.SubProgram where

import Data.Aeson as A (Value(Bool))
import Data.Text
import Data.Typeable
import Data.Vector

import Data.Model
import Data.Model.TH
import Data.Model.View

import Carma.Model.Types (TInt)
import Carma.Model.LegacyTypes (Reference)
import Carma.Model.Program hiding (ident)
import Carma.Model.ServiceNames hiding (ident)

data SubProgram = SubProgram
  { ident        :: PK Int SubProgram ""
  , parent       :: F (IdentI Program)         "parent"    "Программа"
  , label        :: F Text                     "label"     "Название"
  , active       :: F Bool                     "active"    "Активна"
  , value        :: F Text                     "value"     "Внутренняя метка"
  , mailAddr     :: F (Maybe Text)             "mailAddr"  "Mail для отправки писем"
  , mailPass     :: F (Maybe Text)             "mailPass"  "Пароль для отправки писем"
  , contacts     :: F (Maybe Text)             "contacts"  "Контактные лица"
  , services     :: F (Maybe (Vector (IdentI ServiceNames)))
                    "services"  "Услуги, предоставляемые по программе"
  , checkPeriod  :: F (Maybe TInt)
                    "checkPeriod"  "Межсервисный интервал по умолчанию"
  , validFor     :: F (Maybe TInt)
                    "validFor"   "Срок действия программы по умолчанию"
  , contract     :: F (Maybe Reference)        "contract" "Шаблон договора"
  , logo         :: F (Maybe Reference)        "logo" "Логотип"
  , help         :: F (Maybe Text)             "help" "Справка"
  , dealerHelp   :: F (Maybe Text)             "dealerHelp" "Справка для дилеров"
  } deriving Typeable

mkIdents [t|SubProgram|]
 [ ("vwMotor", 1)
 , ("vwCargo", 2)
 , ("peugeot", 3)
 , ("citroen", 4)
 ]

instance Model SubProgram where
  type TableName SubProgram = "SubProgram"
  idents = Carma.Model.SubProgram.idents
  modelInfo = mkModelInfo SubProgram ident
  modelView _ = modifyView defaultView
                [ setMeta "regexp" "number" checkPeriod
                , setMeta "regexp" "number" validFor
                , setMeta "regexp" "email" mailAddr
                , widget "text" checkPeriod
                , widget "text" validFor
                , textarea help
                , textarea dealerHelp
                , setMeta "widget" "inline-uploader" contract
                , setMeta "reference-widget" "files" contract
                , setMeta "widget" "inline-uploader" logo
                , setMeta "reference-widget" "files" logo
                , setMeta "single-uploader" (A.Bool True) logo
                ]
