module Data.Argonaut.Decode.Generic
  ( class DecodeRep
  , class DecodeRepArgs
  , class DecodeLiteral
  , decodeRep
  , decodeRepWith
  , decodeRepArgs
  , genericDecodeJson
  , genericDecodeJsonWith
  , decodeLiteralSum
  , decodeLiteralSumWithTransform
  , decodeLiteral
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut.Core (Json, fromString, toArray, toObject, toString, fromArray)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, JsonDecodeError(..))
import Data.Argonaut.Types.Generic (Encoding, defaultEncoding)
import Data.Array (uncons)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Generic.Rep as Rep
import Data.Symbol (class IsSymbol, reflectSymbol)
import Foreign.Object as FO
import Partial.Unsafe (unsafeCrashWith)
import Prim.TypeError (class Fail, Text)
import Type.Proxy (Proxy(..))

class DecodeRep r where
  decodeRepWith :: Encoding -> Json -> Either JsonDecodeError r

decodeRep :: forall r. DecodeRep r => Json -> Either JsonDecodeError r
decodeRep = decodeRepWith defaultEncoding

instance decodeRepNoConstructors :: DecodeRep Rep.NoConstructors where
  decodeRepWith _ _ = Left $ UnexpectedValue $ fromString "NoConstructors (Cannot decode empty data type)"

instance decodeRepSum :: (DecodeRep a, DecodeRep b) => DecodeRep (Rep.Sum a b) where
  decodeRepWith e j = Rep.Inl <$> decodeRepWith e j <|> Rep.Inr <$> decodeRepWith e j

withTag
  :: Encoding
  -> Json
  -> String
  -> Either JsonDecodeError
       { tag :: String
       , decodingErr :: JsonDecodeError -> JsonDecodeError
       }
withTag e j name = do
  let decodingErr = Named name
  jObj <- note (decodingErr $ TypeMismatch "Object") (toObject j)
  jTag <- note (decodingErr $ AtKey e.tagKey MissingValue) (FO.lookup e.tagKey jObj)
  tag <- note (decodingErr $ AtKey e.tagKey $ TypeMismatch "String") (toString jTag)
  when (tag /= name)
    $ Left
    $ decodingErr
    $ AtKey e.tagKey
    $ UnexpectedValue
    $ fromString tag
  pure { tag, decodingErr }

withTagAndValues
  :: Encoding
  -> Json
  -> String
  -> Either JsonDecodeError
       { tag :: String
       , values :: Json
       , decodingErr :: JsonDecodeError -> JsonDecodeError
       }
withTagAndValues e j name = do
  { tag, decodingErr } <- withTag e j name
  jObj <- note (decodingErr $ TypeMismatch "Object") (toObject j)
  values <- note (decodingErr $ AtKey e.valuesKey MissingValue) (FO.lookup e.valuesKey jObj)
  pure { tag, values, decodingErr }

construct
  :: forall e t s
   . DecodeRepArgs t
  => Encoding
  -> Array Json
  -> (JsonDecodeError -> e)
  -> Either e (Rep.Constructor s t)
construct e valuesArray decodingErr = do
  { init, rest } <- lmap decodingErr $ decodeRepArgs valuesArray
  when (rest /= [])
    $ Left
    $ decodingErr
    $ AtKey e.valuesKey
    $ UnexpectedValue (fromArray rest)
  pure $ Rep.Constructor init

instance decodeRepConstructorNoArgs :: IsSymbol name => DecodeRep (Rep.Constructor name Rep.NoArguments) where
  decodeRepWith e j = do
    let name = reflectSymbol (Proxy :: Proxy name)
    { decodingErr } <- withTag e j name
    construct e [] decodingErr
else instance decodeRepConstructorArg :: (IsSymbol name, DecodeJson a) => DecodeRep (Rep.Constructor name (Rep.Argument a)) where
  decodeRepWith e j = do
    let name = reflectSymbol (Proxy :: Proxy name)
    { values, decodingErr } <- withTagAndValues e j name
    if e.unwrapSingleArguments then construct e [ values ] decodingErr
    else do
      valuesArray <- note (decodingErr $ AtKey e.valuesKey $ TypeMismatch "Array") (toArray values)
      construct e valuesArray decodingErr
else instance decodeRepConstructor :: (IsSymbol name, DecodeRepArgs a) => DecodeRep (Rep.Constructor name a) where
  decodeRepWith e j = do
    let name = reflectSymbol (Proxy :: Proxy name)
    { values, decodingErr } <- withTagAndValues e j name
    valuesArray <- note (decodingErr $ AtKey e.valuesKey $ TypeMismatch "Array") (toArray values)
    construct e valuesArray decodingErr

class DecodeRepArgs r where
  decodeRepArgs :: Array Json -> Either JsonDecodeError { init :: r, rest :: Array Json }

instance decodeRepArgsNoArguments :: DecodeRepArgs Rep.NoArguments where
  decodeRepArgs js = Right { init: Rep.NoArguments, rest: js }

instance decodeRepArgsProduct :: (DecodeRepArgs a, DecodeRepArgs b) => DecodeRepArgs (Rep.Product a b) where
  decodeRepArgs js = do
    { init: a, rest: js' } <- decodeRepArgs js
    { init: b, rest: js'' } <- decodeRepArgs js'
    pure { init: Rep.Product a b, rest: js'' }

instance decodeRepArgsArgument :: (DecodeJson a) => DecodeRepArgs (Rep.Argument a) where
  decodeRepArgs js = do
    { head, tail } <- note (TypeMismatch "NonEmptyArray") (uncons js)
    { init: _, rest: tail } <<< Rep.Argument <$> decodeJson head

-- | Decode `Json` representation of a value which has a `Generic` type.
genericDecodeJson :: forall a r. Rep.Generic a r => DecodeRep r => Json -> Either JsonDecodeError a
genericDecodeJson = genericDecodeJsonWith defaultEncoding

-- | Decode `Json` representation of a value which has a `Generic` type.
-- | Takes a record for encoding settings.
genericDecodeJsonWith :: forall a r. Rep.Generic a r => DecodeRep r => Encoding -> Json -> Either JsonDecodeError a
genericDecodeJsonWith e = map Rep.to <<< decodeRepWith e

-- | A function for decoding `Generic` sum types using string literal representations.
decodeLiteralSum :: forall a r. Rep.Generic a r => DecodeLiteral r => Json -> Either JsonDecodeError a
decodeLiteralSum = decodeLiteralSumWithTransform identity

-- | A function for decoding `Generic` sum types using string literal representations.
-- | Takes a function for transforming the tag name in encoding.
decodeLiteralSumWithTransform :: forall a r. Rep.Generic a r => DecodeLiteral r => (String -> String) -> Json -> Either JsonDecodeError a
decodeLiteralSumWithTransform tagNameTransform = map Rep.to <<< decodeLiteral tagNameTransform

class DecodeLiteral r where
  decodeLiteral :: (String -> String) -> Json -> Either JsonDecodeError r

instance decodeLiteralSumInst :: (DecodeLiteral a, DecodeLiteral b) => DecodeLiteral (Rep.Sum a b) where
  decodeLiteral tagNameTransform j = Rep.Inl <$> decodeLiteral tagNameTransform j <|> Rep.Inr <$> decodeLiteral tagNameTransform j

instance decodeLiteralConstructor :: (IsSymbol name) => DecodeLiteral (Rep.Constructor name (Rep.NoArguments)) where
  decodeLiteral tagNameTransform j = do
    let name = reflectSymbol (Proxy :: Proxy name)
    let decodingErr = Named name
    tag <- note (decodingErr $ TypeMismatch "String") (toString j)
    when (tag /= tagNameTransform name)
      $ Left
      $ decodingErr
      $ UnexpectedValue (fromString tag)
    pure $ Rep.Constructor (Rep.NoArguments)

type FailMessage =
  Text "`decodeLiteralSum` can only be used with sum types, where all of the constructors are nullary. This is because a string literal cannot be encoded into a product type."

instance decodeLiteralConstructorCannotTakeProduct ::
  Fail FailMessage =>
  DecodeLiteral (Rep.Product a b) where
  decodeLiteral _ _ = unsafeCrashWith "unreachable DecodeLiteral was reached."
