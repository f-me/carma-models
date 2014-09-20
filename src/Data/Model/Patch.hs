{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Model.Patch
  ( Patch(Patch), untypedPatch
  , parentField
  , toParentIdent
  , toParentPatch
  , mergeParentPatch
  , IPatch
  , FullPatch, Data.Model.Patch.Object
  , get, get', put, delete, union
  , empty
  , W(..)
  )

where

import Control.Applicative ((<$>))
import Control.Monad.Trans.Reader (ask)
import Control.Monad (mplus)
import Control.Monad.Trans.Class (lift)

import Data.ByteString (ByteString)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Aeson.Types as Aeson
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text, toLower, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Unsafe (unsafeDupablePerformIO)
import qualified Database.PostgreSQL.LibPQ as PQ
import Database.PostgreSQL.Simple.Internal ( Row(..)
                                           , RowParser(..)
                                           , conversionError)
import Database.PostgreSQL.Simple.FromField (ResultError(..))
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow

import Data.Dynamic

import Data.Singletons

import Data.Model


data Patch m -- FIXME: why HashMap, not good old Data.Map?
  = Patch { untypedPatch :: HashMap Text Dynamic }
  deriving Typeable


-- | A version of 'Patch' guaranteed to have all fields.
newtype FullPatch m =
  FullPatch (Patch m) deriving (IPatch, ToJSON, ToRow, Typeable)


type Object m = FullPatch m


class IPatch p where
  -- | Look up a field in a patch.
  get :: (Typeable t, SingI name) =>
         p m -> (m -> Field t (FOpt name desc app)) -> Maybe t

  -- | Add a field to a patch.
  put :: (Typeable t, SingI name) =>
         (m -> Field t (FOpt name desc app)) -> t -> p m -> p m

  -- | Delete a field from a patch.
  delete :: (SingI name) =>
            (m -> Field t (FOpt name desc app)) -> p m -> Patch m


empty :: Patch m
empty = Patch HashMap.empty


instance IPatch Patch where
  get (Patch m) f = (`fromDyn` (error "Dynamic error in patch")) <$>
                    HashMap.lookup (fieldName f) m

  put f v (Patch m) = Patch $ HashMap.insert (fieldName f) (toDyn v) m

  delete f (Patch m) = Patch $ HashMap.delete (fieldName f) m


-- | Type-safe total version of 'get'.
get' :: (Typeable t, SingI name) =>
        FullPatch m -> (m -> Field t (FOpt name desc app)) -> t
get' p f
  = fromMaybe (error $ "Patch field " ++ unpack (fieldName f) ++ " missing") $
    get p f


union :: Patch m -> Patch m -> Patch m
union p1 p2 = Patch $ HashMap.union (untypedPatch p1) (untypedPatch p2)

parentField :: Model m =>
               (Parent m -> Field t (FOpt name desc app))
            -> (m -> Field t (FOpt name desc app))
parentField _ _ = Field


toParentIdent
  :: Model m
  => Ident t m -> Ident t (Parent m)
toParentIdent = Ident . identVal


toParentPatch
  :: Model m
  => Patch m -> Patch (Parent m)
toParentPatch = Patch . untypedPatch


mergeParentPatch
  :: forall m . Model m
  => Patch m -> Patch (Parent m) -> Patch m
mergeParentPatch a b = case parentInfo :: ParentInfo m of
  NoParent   -> a
  ExParent p ->
    let ua = untypedPatch a
        ub = untypedPatch b
        fs = modelFieldsMap p
        ub'= HashMap.filterWithKey (\k _ -> HashMap.member k fs) ub
    in Patch $ HashMap.union ub' ua


instance Model m => FromJSON (Patch m) where
  parseJSON (Aeson.Object o)
    = Patch . HashMap.fromList
    <$> mapM parseField (HashMap.toList o)
    where
      fields = modelFieldsMap (modelInfo :: ModelInfo m)
      parseField (name, val) = case HashMap.lookup name fields of
        Nothing -> fail $ "Unexpected field: " ++ show name
        Just p  -> (name,) <$> fd_parseJSON p val
  parseJSON j = fail $ "JSON object expected but here is what I have: " ++ show j


instance Model m => ToJSON (Patch m) where
  toJSON (Patch m) = object [(k, toJS k v) | (k,v) <- HashMap.toList m]
    where
      fields = modelFieldsMap (modelInfo :: ModelInfo m)
      toJS k = fd_toJSON $ fields HashMap.! k


instance Model m => FromRow (Patch m) where
  fromRow = Patch . HashMap.fromList <$> sequence
    [(fd_name f,) <$> fd_fromField f
    | f <- onlyDefaultFields $ modelFields (modelInfo :: ModelInfo m)
    ]

instance Model m => ToRow (Patch m) where
  toRow (Patch m) = concatMap fieldToRow $ HashMap.toList m
    where
      -- NB. Please note that field order is significant
      -- it MUST match with the one in Patch.Sql.insert
      fields = modelFieldsMap (modelInfo :: ModelInfo m)
      fieldToRow (nm, val) = case HashMap.lookup nm fields of
        Just f@(FieldDesc{}) -> [fd_toField f val]
        Just _  -> [] -- skip ephemeral field
        Nothing -> [] -- skip unknown fields to allow upcasting child models

newtype W m = W { unW :: m }

-- | Special instance which can build patch retrieving fields by their names
instance Model m => FromRow (W (Patch m)) where
  fromRow = do
    n  <- numFieldsRemaining
    fs <- map decodeUtf8 <$> catMaybes <$> mapM fname [0..n-1]
    case filter (\(_, f) -> not $ hasField f) $ zip fs $ fields fs of
      [] -> W . Patch . HashMap.fromList <$> sequence
            [(fd_name f,) <$> fd_fromField f | f <- catMaybes $ fields fs]
      errs -> RP $ lift $ lift $ conversionError $
              ConversionFailed  "" Nothing "" "" $
        "Can't find this fields in model: " ++ (show $ map fst errs)
    where
      fM = modelFieldsMap (modelInfo :: ModelInfo m)
      fm = HashMap.foldl'
           (\a f -> HashMap.insert (toLower $ fd_name f) f a) HashMap.empty fM
      fields = map (\n -> HashMap.lookup n fM `mplus` HashMap.lookup n fm)
      hasField (Just _) = True
      hasField Nothing  = False

fname :: Int -> RowParser (Maybe ByteString)
fname n = RP $ do
  Row{..} <- ask
  return $ unsafeDupablePerformIO $ PQ.fname rowresult (PQ.toColumn n)

instance Model m => ToJSON (W (Patch m)) where
  toJSON (W p) = toJSON p


instance Model m => FromRow (FullPatch m) where
  fromRow = do
    let rowP :: RowParser (W (Patch m))
        rowP = fromRow
    p <- rowP
    return $ FullPatch $ unW p
