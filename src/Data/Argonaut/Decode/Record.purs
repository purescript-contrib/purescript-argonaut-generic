module Data.Argonaut.Decode.Record
  ( class DecodeFields
  , decodeRecord
  , decodeFields
  ) where

import Prelude

import Data.Argonaut.Core (JObject, Json, toObject)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Either (Either, note)
import Data.Record as Rec
import Data.StrMap as SM
import Type.Prelude (class IsSymbol, class RowLacks, class TypeEquals, RProxy(RProxy), SProxy(SProxy), from, reflectSymbol)
import Type.Row (class RowToList, Cons, Nil, RLProxy(..), kind RowList)

-- | Decode a record from `Json` using inner `DecodeJson` instance.
decodeRecord :: forall row rowList
   . RowToList row rowList
  => DecodeFields rowList row
  => Json
  -> Either String (Record row)
decodeRecord j = do
  obj <- note "Could not get JObject from Json" $ toObject j
  decodeFields (RLProxy :: RLProxy rowList) (RProxy :: RProxy row) obj

class DecodeFields (xs :: RowList) (row :: # Type) where
  decodeFields :: RLProxy xs -> RProxy row -> JObject -> Either String (Record row)

instance decodeFieldsCons ::
  ( IsSymbol name
  , DecodeJson ty
  , DecodeFields tail tailRow
  , RowLacks name tailRow
  , RowCons name ty tailRow row
  ) => DecodeFields (Cons name ty tail) row where
  decodeFields _ _ j = do
    rest <- decodeFields (RLProxy :: RLProxy tail) (RProxy :: RProxy tailRow) j
    lookup <- note ("property " <> name <> " is missing") $ SM.lookup name j
    value <- decodeJson lookup
    pure $ Rec.insert nameP value rest
    where
      nameP = SProxy :: SProxy name
      name = reflectSymbol nameP

instance decodeFieldsNil ::
  ( TypeEquals (Record row) {}
  ) => DecodeFields Nil row where
  decodeFields _ _ _ = pure $ from {}
