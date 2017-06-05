module Data.Argonaut.Encode.Generic (
  gEncodeJson,
  gEncodeJson',

  class EncodeRep,
  class EncodeRepArgs,
  class EncodeRepFields,
  encodeRep,
  encodeRepArgs,
  encodeRepFields,
  genericEncodeJson
) where

import Prelude

import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Core (Json(), jsonNull, fromBoolean, fromNumber, fromString, fromArray, fromObject)
import Data.Int (toNumber)
import Data.Foldable (foldr)
import Data.Generic (class Generic, GenericSpine(..), toSpine)
import Data.Generic.Rep as Rep
import Data.String (singleton)
import Data.StrMap as SM
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)

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

class EncodeRep r where
  encodeRep :: r -> Json

instance encodeRepNoConstructors :: EncodeRep Rep.NoConstructors where
  encodeRep r = encodeRep r

instance encodeRepSum :: (EncodeRep a, EncodeRep b) => EncodeRep (Rep.Sum a b) where
  encodeRep (Rep.Inl a) = encodeRep a
  encodeRep (Rep.Inr b) = encodeRep b

instance encodeRepConstructor :: (IsSymbol name, EncodeRepArgs a) => EncodeRep (Rep.Constructor name a) where
  encodeRep (Rep.Constructor a) =
    fromObject
      $ SM.insert "tag" (fromString (reflectSymbol (SProxy :: SProxy name)))
      $ SM.insert "values" (fromArray (encodeRepArgs a))
      $ SM.empty

class EncodeRepArgs r where
  encodeRepArgs :: r -> Array Json

instance encodeRepArgsNoArguments :: EncodeRepArgs Rep.NoArguments where
  encodeRepArgs Rep.NoArguments = []

instance encodeRepArgsProduct :: (EncodeRepArgs a, EncodeRepArgs b) => EncodeRepArgs (Rep.Product a b) where
  encodeRepArgs (Rep.Product a b) = encodeRepArgs a <> encodeRepArgs b

instance encodeRepArgsArgument :: (EncodeJson a) => EncodeRepArgs (Rep.Argument a) where
  encodeRepArgs (Rep.Argument a) = [encodeJson a]

instance encodeRepArgsRec :: (EncodeRepFields fields) => EncodeRepArgs (Rep.Rec fields) where
  encodeRepArgs (Rep.Rec fields) = [fromObject $ encodeRepFields fields]

class EncodeRepFields r where
  encodeRepFields :: r -> SM.StrMap Json

instance encodeRepFieldsProduct :: (EncodeRepFields a, EncodeRepFields b) => EncodeRepFields (Rep.Product a b) where
  encodeRepFields (Rep.Product a b) =
    SM.union (encodeRepFields a) (encodeRepFields b)

instance encodeRepFieldsField :: (IsSymbol field, EncodeJson a) => EncodeRepFields (Rep.Field field a) where
  encodeRepFields (Rep.Field a) =
    SM.singleton (reflectSymbol (SProxy :: SProxy field))
                 (encodeJson a)

genericEncodeJson :: forall a r. Rep.Generic a r => EncodeRep r => a -> Json
genericEncodeJson = encodeRep <<< Rep.from
