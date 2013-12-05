{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.Program where

import Data.Text
import Data.Typeable
import Data.Model
import Data.Model.View
import Data.Model.TH

import Carma.Model.Types()

data Program = Program
  { ident                 :: PK Int Program ""
  , label                 :: F Text            "label"  "Название"
  , client                :: F (Maybe Text)    "client" "Заказчик"
  , clientAddress         :: F (Maybe Text)    "clientAddress" "Адрес заказчика"
  , clientCode            :: F (Maybe Text)    "clientCode" "Код заказчика"
  } deriving Typeable

instance Model Program where
  type TableName Program = "Program"
  modelInfo = mkModelInfo Program ident
  modelView _ = defaultView

mkIdents [t|Program|]
 [ ("peugeot", 1)
 , ("citroen", 2)
 , ("vtb24", 3)
 ]
