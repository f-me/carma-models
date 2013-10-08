
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Model.View
  (defaultView
  ,defaultFieldView
  ,modifyView
  ,textarea
  ,readonly
  -- from Data.Model.View.Types
  ,FieldView(..)
  ,ModelView(..)
  ) where

import Data.List
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Aeson as Aeson
import Data.Typeable

import GHC.TypeLits

import Data.Vector (Vector)

import Data.Model as Model
import Data.Model.Sql
import Data.Model.View.Types

defaultView :: forall m . Model m => ModelView m
defaultView = ModelView
  { modelName = Model.modelName (modelInfo :: ModelInfo m)
  , title = ""
  , fields
    = [defaultFieldView f
      | f <- modelFields (modelInfo :: ModelInfo m)
      , fd_name f /= "id"]
  }


defaultFieldView :: FieldDesc m -> FieldView m
defaultFieldView f = FieldView
  { name      = fd_name f
  , fieldType =  (\FieldDesc{fd_realType = t} -> translateFieldType t) f
  , canWrite  = True
  , meta      = Map.fromList
    $  [("label", Aeson.String $ fd_desc f)]
    ++ case fd_name f of
      "id" -> [("invisible", Aeson.Bool True)]
      _ -> []
    ++ case words $ show $ fd_type f of
      ["Ident", model] ->
        [("dictionaryName", Aeson.String $ T.pack model)
        ,("dictionaryType", "ModelDict")
        ,("bounded", Aeson.Bool True)
        ]
      ["Vector", "(Ident", model] ->
        [("dictionaryName", Aeson.String
          $ fromJust $ T.stripSuffix ")" $ T.pack model)
        ,("dictionaryType", "ModelDict")
        ,("bounded", Aeson.Bool True)
        ,("widget", "dictionary-many")
        ]
      _ -> []
  }


modifyView
  :: ModelView m -> [(Text, FieldView m -> FieldView m)]
  -> ModelView m
modifyView mv@(ModelView{fields}) fns
  = mv {fields = map ((fMap' Map.!) . name) fields}
  where
    fMap = Map.fromList [(name f, f) | f <- fields]
    fMap' = foldl' tr fMap fns
    tr m (nm,fn) = Map.adjust fn nm m


-- field modificators

textarea
  :: SingI name => (model -> Field Text (FOpt name desc))
  -> (Text, FieldView m -> FieldView m)
textarea fld
  = (fieldName fld, \v -> v {fieldType = "textarea"})


readonly
  :: SingI name => (model -> Field typ (FOpt name desc))
  -> (Text, FieldView m -> FieldView m)
readonly fld
  = (fieldName fld
    ,\v -> v
      {meta = Map.insert "readonly" (Aeson.Bool True) $ meta v
      ,canWrite = False
      }
    )
