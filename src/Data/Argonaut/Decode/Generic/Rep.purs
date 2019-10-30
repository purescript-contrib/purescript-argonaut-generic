module Data.Argonaut.Decode.Generic.Rep (
  class DecodeRep,
  class DecodeRepArgs,
  class DecodeLiteral,
  decodeRep,
  decodeRepWith,
  decodeRepArgs,
  genericDecodeJson,
  genericDecodeJsonWith,
  decodeLiteralSum,
  decodeLiteralSumWithTransform,
  decodeLiteral
) where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut.Core (Json, toArray, toObject, toString)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Types.Generic.Rep (Encoding, defaultEncoding)
import Data.Array (uncons)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Generic.Rep as Rep
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Foreign.Object as FO
import Partial.Unsafe (unsafeCrashWith)
import Prim.TypeError (class Fail, Text)

class DecodeRep r where
  decodeRepWith :: Encoding -> Json -> Either String r

decodeRep :: forall r. DecodeRep r => Json -> Either String r
decodeRep = decodeRepWith defaultEncoding

instance decodeRepNoConstructors :: DecodeRep Rep.NoConstructors where
  decodeRepWith e _ = Left "Cannot decode empty data type"

instance decodeRepSum :: (DecodeRep a, DecodeRep b) => DecodeRep (Rep.Sum a b) where
  decodeRepWith e j = Rep.Inl <$> decodeRepWith e j <|> Rep.Inr <$> decodeRepWith e j

withTag ::
  Encoding ->
  Json ->
  String ->
  Either String
    { tag :: String
    , decodingErr :: String -> String
    }
withTag e j name = do
  let decodingErr msg = "When decoding a " <> name <> ": " <> msg
  jObj <- note (decodingErr "expected an object") (toObject j)
  jTag <- note (decodingErr $ "'" <> e.tagKey <> "' property is missing") (FO.lookup e.tagKey jObj)
  tag <- note (decodingErr $ "'" <> e.tagKey <> "' property is not a string") (toString jTag)
  when (tag /= name) $
    Left $ decodingErr $ "'" <> e.tagKey <> "' property has an incorrect value"
  pure {tag, decodingErr}

withTagAndValues ::
  Encoding ->
  Json ->
  String ->
  Either String
    { tag :: String
    , values :: Json
    , decodingErr :: String -> String
    }
withTagAndValues e j name = do
  {tag, decodingErr} <- withTag e j name
  jObj <- note (decodingErr "expected an object") (toObject j)
  values <- note (decodingErr $ "'" <> e.valuesKey <> "' property is missing") (FO.lookup e.valuesKey jObj)
  pure {tag, values, decodingErr}

construct ::
  forall e t s .
  DecodeRepArgs t =>
  Encoding ->
  Array Json ->
  (String -> e) ->
  Either e (Rep.Constructor s t)
construct e valuesArray decodingErr = do
  {init, rest} <- lmap decodingErr $ decodeRepArgs valuesArray
  when (rest /= []) $
    Left $ decodingErr $ "'" <> e.valuesKey <> "' property had too many values"
  pure $ Rep.Constructor init

instance decodeRepConstructorNoArgs :: IsSymbol name => DecodeRep (Rep.Constructor name Rep.NoArguments) where
  decodeRepWith e j = do
    let name = reflectSymbol (SProxy :: SProxy name)
    {tag, decodingErr} <- withTag e j name
    construct e [] decodingErr
else
instance decodeRepConstructorArg :: (IsSymbol name, DecodeJson a) => DecodeRep (Rep.Constructor name (Rep.Argument a)) where
  decodeRepWith e j = do
    let name = reflectSymbol (SProxy :: SProxy name)
    {tag, values, decodingErr} <- withTagAndValues e j name
    if e.unwrapSingleArguments
      then construct e [values] decodingErr
      else do
        valuesArray <- note (decodingErr $ "'" <> e.valuesKey <> "' property is not an array") (toArray values)
        construct e valuesArray decodingErr
else
instance decodeRepConstructor :: (IsSymbol name, DecodeRepArgs a) => DecodeRep (Rep.Constructor name a) where
  decodeRepWith e j = do
    let name = reflectSymbol (SProxy :: SProxy name)
    {tag, values, decodingErr} <- withTagAndValues e j name
    valuesArray <- note (decodingErr $ "'" <> e.valuesKey <> "' property is not an array") (toArray values)
    construct e valuesArray decodingErr

class DecodeRepArgs r where
  decodeRepArgs :: Array Json -> Either String {init :: r, rest :: Array Json}

instance decodeRepArgsNoArguments :: DecodeRepArgs Rep.NoArguments where
  decodeRepArgs js = Right {init: Rep.NoArguments, rest: js}

instance decodeRepArgsProduct :: (DecodeRepArgs a, DecodeRepArgs b) => DecodeRepArgs (Rep.Product a b) where
  decodeRepArgs js = do
    {init: a, rest: js'} <- decodeRepArgs js
    {init: b, rest: js''} <- decodeRepArgs js'
    pure {init: Rep.Product a b, rest: js''}

instance decodeRepArgsArgument :: (DecodeJson a) => DecodeRepArgs (Rep.Argument a) where
  decodeRepArgs js = do
    {head, tail} <- note "too few values were present" (uncons js)
    {init: _, rest: tail} <<< Rep.Argument <$> decodeJson head

-- | Decode `Json` representation of a value which has a `Generic` type.
genericDecodeJson :: forall a r. Rep.Generic a r => DecodeRep r => Json -> Either String a
genericDecodeJson = genericDecodeJsonWith defaultEncoding

-- | Decode `Json` representation of a value which has a `Generic` type.
-- | Takes a record for encoding settings.
genericDecodeJsonWith :: forall a r. Rep.Generic a r => DecodeRep r => Encoding -> Json -> Either String a
genericDecodeJsonWith e = map Rep.to <<< decodeRepWith e

-- | A function for decoding `Generic` sum types using string literal representations.
decodeLiteralSum :: forall a r. Rep.Generic a r => DecodeLiteral r => Json -> Either String a
decodeLiteralSum = decodeLiteralSumWithTransform identity

-- | A function for decoding `Generic` sum types using string literal representations.
-- | Takes a function for transforming the tag name in encoding.
decodeLiteralSumWithTransform :: forall a r. Rep.Generic a r => DecodeLiteral r => (String -> String) -> Json -> Either String a
decodeLiteralSumWithTransform tagNameTransform = map Rep.to <<< decodeLiteral tagNameTransform

class DecodeLiteral r where
  decodeLiteral :: (String -> String) -> Json -> Either String r

instance decodeLiteralSumInst :: (DecodeLiteral a, DecodeLiteral b) => DecodeLiteral (Rep.Sum a b) where
  decodeLiteral tagNameTransform j = Rep.Inl <$> decodeLiteral tagNameTransform j <|> Rep.Inr <$> decodeLiteral tagNameTransform j

instance decodeLiteralConstructor :: (IsSymbol name) => DecodeLiteral (Rep.Constructor name (Rep.NoArguments)) where
  decodeLiteral tagNameTransform j = do
    let name = reflectSymbol (SProxy :: SProxy name)
    let decodingErr msg = "When decoding a " <> name <> ": " <> msg
    tag <- note (decodingErr "could not read string for constructor") (toString j)
    when (tag /= tagNameTransform name) $
      Left $ decodingErr "string literal " <> tag <> " had an incorrect value."
    pure $ Rep.Constructor (Rep.NoArguments)


type FailMessage =
  Text "`decodeLiteralSum` can only be used with sum types, where all of the constructors are nullary. This is because a string literal cannot be encoded into a product type."

instance decodeLiteralConstructorCannotTakeProduct
  :: Fail FailMessage
  => DecodeLiteral (Rep.Product a b) where
    decodeLiteral _ _ = unsafeCrashWith "unreachable DecodeLiteral was reached."
