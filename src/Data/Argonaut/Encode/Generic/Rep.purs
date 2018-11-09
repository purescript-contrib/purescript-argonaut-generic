module Data.Argonaut.Encode.Generic.Rep (
  class EncodeRep,
  class EncodeRepArgs,
  class EncodeRepFields,
  class EncodeLiteral,
  encodeRepWith,
  encodeRepArgs,
  encodeRepFields,
  genericEncodeJson,
  genericEncodeJsonWith,
  encodeLiteralSum,
  encodeLiteralSumWithTransform,
  encodeLiteral
) where

import Prelude
import Data.Argonaut.Types.Generic.Rep (Encoding, defaultEncoding)

import Data.Argonaut.Core (Json, fromArray, fromObject, fromString)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Generic.Rep as Rep
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Foreign.Object as FO
import Partial.Unsafe (unsafeCrashWith)
import Prim.TypeError (class Fail, Text)

class EncodeRep r where
  encodeRepWith :: Encoding -> r -> Json

instance encodeRepNoConstructors :: EncodeRep Rep.NoConstructors where
  encodeRepWith e = encodeRepWith e

instance encodeRepSum :: (EncodeRep a, EncodeRep b) => EncodeRep (Rep.Sum a b) where
  encodeRepWith e (Rep.Inl a) = encodeRepWith e a
  encodeRepWith e (Rep.Inr b) = encodeRepWith e b

instance encodeRepConstructor :: (IsSymbol name, EncodeRepArgs a) => EncodeRep (Rep.Constructor name a) where
  encodeRepWith e (Rep.Constructor a) =
    fromObject
      $ FO.insert e.tagKey (fromString (reflectSymbol (SProxy :: SProxy name)))
      $ FO.insert e.valuesKey (fromArray (encodeRepArgs a))
      $ FO.empty

class EncodeRepArgs r where
  encodeRepArgs :: r -> Array Json

instance encodeRepArgsNoArguments :: EncodeRepArgs Rep.NoArguments where
  encodeRepArgs Rep.NoArguments = []

instance encodeRepArgsProduct :: (EncodeRepArgs a, EncodeRepArgs b) => EncodeRepArgs (Rep.Product a b) where
  encodeRepArgs (Rep.Product a b) = encodeRepArgs a <> encodeRepArgs b

instance encodeRepArgsArgument :: (EncodeJson a) => EncodeRepArgs (Rep.Argument a) where
  encodeRepArgs (Rep.Argument a) = [encodeJson a]

class EncodeRepFields r where
  encodeRepFields :: r -> FO.Object Json

instance encodeRepFieldsProduct :: (EncodeRepFields a, EncodeRepFields b) => EncodeRepFields (Rep.Product a b) where
  encodeRepFields (Rep.Product a b) =
    FO.union (encodeRepFields a) (encodeRepFields b)


-- | Encode any `Generic` data structure into `Json`.
genericEncodeJson :: forall a r. Rep.Generic a r => EncodeRep r => a -> Json
genericEncodeJson = genericEncodeJsonWith defaultEncoding

-- | Encode any `Generic` data structure into `Json`.
genericEncodeJsonWith :: forall a r. Rep.Generic a r => EncodeRep r => Encoding -> a -> Json
genericEncodeJsonWith e = encodeRepWith e <<< Rep.from

-- | A function for encoding `Generic` sum types using string literal representations
encodeLiteralSum :: forall a r. Rep.Generic a r => EncodeLiteral r => a -> Json
encodeLiteralSum = encodeLiteralSumWithTransform identity

-- | A function for encoding `Generic` sum types using string literal representations
-- | Takes a function for transforming the tag name in encoding
encodeLiteralSumWithTransform :: forall a r. Rep.Generic a r => EncodeLiteral r => (String -> String) -> a -> Json
encodeLiteralSumWithTransform tagNameTransform = encodeLiteral tagNameTransform <<< Rep.from

class EncodeLiteral r where
  encodeLiteral :: (String -> String) -> r -> Json

instance encodeLiteralSumInst :: (EncodeLiteral a, EncodeLiteral b) => EncodeLiteral (Rep.Sum a b) where
  encodeLiteral tagNameTransform (Rep.Inl a) = encodeLiteral tagNameTransform a
  encodeLiteral tagNameTransform (Rep.Inr b) = encodeLiteral tagNameTransform b

instance encodeLiteralConstructor :: (IsSymbol name) => EncodeLiteral (Rep.Constructor name (Rep.NoArguments)) where
  encodeLiteral tagNameTransform _ = fromString <<< tagNameTransform $ reflectSymbol (SProxy :: SProxy name)

type FailMessage = 
  Text """`encodeLiteralSum` can only be used with sum types, where all of the constructors are nullary. This is because a string literal cannot be encoded into a product type."""

instance encodeLiteralConstructorCannotBeProduct
  :: Fail FailMessage
  => EncodeLiteral (Rep.Product a b) where
  encodeLiteral _ _ = unsafeCrashWith "unreachable encodeLiteral was reached."
