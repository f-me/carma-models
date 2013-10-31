
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Model.Patch.Sql
  (create
  ,read
  ,readMany
  ,update
  ) where

import Prelude hiding (read)

import Control.Applicative
import Data.Int (Int64)
import Data.String
import Text.Printf

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HashMap
import Database.PostgreSQL.Simple

import Data.Model
import Data.Model.Patch


create :: forall m . Model m => Patch m -> Connection -> IO (IdentI m)
create p c = head . head <$> query c (fromString q) p
  where
    mInfo = modelInfo :: ModelInfo m
    m = untypedPatch p
    -- we use `map fst . HashMap.toList` instead of `HashMap.keys`
    -- just to be sure that `insFields` are in the same order as
    -- `ToRow (Patch m)` expects
    insFields = map fst $ HashMap.toList m
    q = printf "INSERT INTO %s (%s) VALUES (%s) RETURNING id"
      (show $ tableName mInfo)
      (T.unpack $ T.intercalate ", " insFields)
      (T.unpack $ T.intercalate ", " $ replicate (length insFields) "?")


read :: forall m . Model m => IdentI m -> Connection -> IO [Patch m]
read (Ident i) c = query c (fromString q) [i]
  where
    mInfo = modelInfo :: ModelInfo m
    fieldNames = map fd_name $ modelFields mInfo
    q = printf "SELECT %s FROM %s WHERE id = ?"
      (T.unpack $ T.intercalate ", " fieldNames)
      (show $ tableName mInfo)


readMany :: forall m . Model m => Int64 -> Int64 -> Connection -> IO [Patch m]
readMany lim off c = query_ c (fromString q)
  where
    mInfo = modelInfo :: ModelInfo m
    fieldNames = map fd_name $ modelFields mInfo
    q = printf "SELECT %s FROM %s ORDER BY id LIMIT %i OFFSET %i"
      (T.unpack $ T.intercalate ", " fieldNames)
      (show $ tableName mInfo)
      lim off


update :: forall m . Model m => IdentI m -> Patch m -> Connection -> IO Int64
update (Ident i) p c = execute c (fromString q) p
  where
    mInfo = modelInfo :: ModelInfo m
    m = untypedPatch p
    -- we use `map fst . HashMap.toList` instead of `HashMap.keys`
    -- just to be sure that `insFields` are in the same order as
    -- `ToRow (Patch m)` expects
    updFields = map (T.concat . (:["=?"]) . fst) $ HashMap.toList m
    q = printf "UPDATE %s SET %s WHERE id = %d"
      (show $ tableName mInfo)
      (T.unpack $ T.intercalate ", " updFields)
      i
