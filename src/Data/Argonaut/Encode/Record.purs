module Data.Argonaut.Encode.Record
  ( class EncodeFields
  , encodeRecord
  , encodeFields
  ) where

import Prelude

import Data.Argonaut.Core (Json, JObject, fromObject)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Record as Rec
import Data.StrMap as SM
import Type.Prelude (class IsSymbol, class RowLacks, SProxy(SProxy), reflectSymbol)
import Type.Row (class RowToList, Cons, Nil, RLProxy(..), kind RowList)

-- | Encode a record into `Json` using inner `EncodeJson` instances.
encodeRecord :: forall row rowList
   . RowToList row rowList
  => EncodeFields rowList row
  => Record row
  -> Json
encodeRecord r = fromObject $ encodeFields (RLProxy :: RLProxy rowList) r

class EncodeFields (xs :: RowList) (row :: # Type) where
  encodeFields :: RLProxy xs -> Record row -> JObject

instance encodeFieldsCons ::
  ( IsSymbol name
  , EncodeJson ty
  , EncodeFields tail tailRow
  , RowLacks name tailRow
  , RowCons name ty tailRow row
  ) => EncodeFields (Cons name ty tail) row where
  encodeFields _ record =
    SM.insert (reflectSymbol nameP) (encodeJson $ Rec.get nameP record)
      $ encodeFields (RLProxy :: RLProxy tail) tailRecord
    where
      nameP = SProxy :: SProxy name
      tailRecord :: Record tailRow
      tailRecord = Rec.delete nameP record

instance encodeFieldsNil :: EncodeFields Nil row where
  encodeFields _ _ = SM.empty
