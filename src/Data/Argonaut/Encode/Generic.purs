module Data.Argonaut.Encode.Generic (
  gEncodeJson,
  gEncodeJson'
) where

import Prelude

import Data.Argonaut.Encode.Class (encodeJson)
import Data.Argonaut.Core (Json(), jsonNull, fromBoolean, fromNumber, fromString, fromArray, fromObject)
import Data.Int (toNumber)
import Data.Foldable (foldr)
import Data.Generic (class Generic, GenericSpine(..), toSpine)
import Data.String (singleton)
import Data.StrMap as SM

-- | Encode any `Generic` data structure into `Json`.
gEncodeJson :: forall a. Generic a => a -> Json
gEncodeJson = gEncodeJson' <<< toSpine

-- | Encode `GenericSpine` into `Json`.
gEncodeJson' :: GenericSpine -> Json
gEncodeJson' = case _ of
  SInt x -> fromNumber $ toNumber x
  SString x -> fromString x
  SChar x -> fromString $ singleton x
  SNumber x -> fromNumber x
  SBoolean x -> fromBoolean x
  SArray thunks -> fromArray (gEncodeJson' <<< (unit # _) <$> thunks)
  SUnit -> jsonNull
  SProd constr args ->
    fromObject
      $ SM.insert "tag" (encodeJson constr)
      $ SM.singleton "values" (encodeJson (gEncodeJson' <<< (unit # _) <$> args))
  SRecord fields ->
    fromObject $ foldr addField SM.empty fields
    where
    addField field =
      SM.insert field.recLabel (gEncodeJson' $ field.recValue unit)
