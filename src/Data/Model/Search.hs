
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Model.Search where

import Control.Applicative
import Control.Exception

import Data.Either (partitionEithers)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import Data.Vector (Vector)

import Data.Time.Calendar (Day)

import Data.Aeson as Aeson
import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.ToField (ToField)

import Text.Printf
import GHC.TypeLits

import Data.Model as Model
import Data.Model.View
import Carma.Model.Types

data Predicate m
  = Predicate
    { tableName :: Text
    , fieldName :: Text
    , fieldDesc :: FieldDesc m
    , matchType :: MatchType
    , escapeVal
      :: PG.Connection -> PG.Query -> Aeson.Value
      -> IO (Either String Text)
    }

data MatchType
  = MatchExact | MatchFuzzy | MatchArray | MatchInterval
  deriving Show


-- FIXME: check if field type \in {Text, Int, ..}
one
  :: forall m t nm desc
  . (FromJSON t, ToField t
    ,SingI nm, SingI desc, Model m)
  => (m -> F t nm desc) -> [Predicate m]
one f = Predicate
  { tableName = Model.tableName (modelInfo :: ModelInfo m)
  , fieldName = Model.fieldName f
  , fieldDesc = modelFieldsMap modelInfo HM.! Model.fieldName f
  , matchType = MatchExact
  , escapeVal = \conn qTpl val ->
      case fromJSON val :: Aeson.Result t of
        Error err -> return $ Left $ "Aeson error: " ++ err
        Success pgVal
          -> try (PG.formatQuery conn qTpl (Only pgVal))
          >>= return . \case
            Left e  -> Left $ show (e :: PG.FormatError)
            Right q -> Right $ T.decodeUtf8 q
  } : []


listOf
  :: forall m t nm desc
  . (FromJSON t, ToField t
    ,SingI nm, SingI desc, Model m)
  => (m -> F t nm desc) -> [Predicate m]
listOf _
  = map (\p -> p {matchType = MatchArray})
  $ one (undefined :: m -> F (Vector t) nm desc)


fuzzy :: [Predicate m] -> [Predicate m]
fuzzy = map (\p -> p {matchType = MatchFuzzy})


matchAny :: [[Predicate m]] -> [Predicate m]
matchAny = concat

interval
 :: forall m nm desc
 . (SingI nm, SingI desc, Model m)
 => (m -> F Day nm desc) -> [Predicate m]
interval _
 = map (\p -> p {matchType = MatchInterval})
 $ one (undefined :: m -> F (Interval Day) nm desc)

renderPredicate
  :: PG.Connection -> HashMap Text [Predicate m] -> Aeson.Object
  -> IO (Either String Text)
renderPredicate conn pMap vals = do
  let parenthize s = T.concat ["(", s, ")"]

  let renderDisjunct v (Predicate{..})
        = (\q -> escapeVal conn (fromString q) v)
        $ case matchType of
          MatchExact
            -> printf "%s.%s = ?"
              (show tableName) (T.unpack fieldName)
          MatchFuzzy
            -> printf "%s.%s ilike ('%%' || ? || '%%')"
              (show tableName) (T.unpack fieldName)
          MatchArray
            -> printf "%s.%s = ANY(?)"
              (show tableName) (T.unpack fieldName)
          MatchInterval
            -> printf "%s.%s <@ ?"
               (show tableName) (T.unpack fieldName)

  let renderConjunct (key,val) = case HM.lookup key pMap of
        Nothing -> return $ Left
          $ "Invalid search param: " ++ show (key,val)
        Just ps -> do
          djs <- mapM (renderDisjunct val) ps
          return $ case partitionEithers djs of
            (errs@(_:_), _) -> Left $ show errs
            ([], res)       -> Right $ T.intercalate " OR " $ map parenthize res

  cjs <- mapM renderConjunct $ HM.toList vals
  return $ case partitionEithers cjs of
    (errs@(_:_), _) -> Left $ show errs
    ([], res)       -> Right $ T.intercalate " AND " $ map parenthize res


searchView :: [(Text, [Predicate m])] -> ModelView m
searchView flds = ModelView
  { modelName = "search"
  , title = "Поиск"
  , fields
    = [ v
        {name = nm
        ,meta = Map.insert
          "matchType"
          (Aeson.String $ T.pack $ show $ matchType p)
          (meta v)
        }
      | (nm,p:_) <- flds
      , let v = defaultFieldView $ fieldDesc p
      ]
  }
