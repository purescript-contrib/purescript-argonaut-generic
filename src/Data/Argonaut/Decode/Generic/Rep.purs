module Data.Argonaut.Decode.Generic.Rep (
  class DecodeRep,
  class DecodeRepArgs,
  class DecodeLiteral,
  decodeRep,
  decodeRepArgs,
  genericDecodeJson,
  decodeLiteralSum,
  decodeLiteralSumWithTransform,
  decodeLiteral
) where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut.Core (Json, toArray, toObject, toString)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Array (uncons)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Generic.Rep as Rep
import Data.Maybe (Maybe, maybe)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Foreign.Object as FO
import Partial.Unsafe (unsafeCrashWith)
import Prim.TypeError (class Fail, Text)

class DecodeRep r where
  decodeRep :: Json -> Either String r

instance decodeRepNoConstructors :: DecodeRep Rep.NoConstructors where
  decodeRep _ = Left "Cannot decode empty data type"

instance decodeRepSum :: (DecodeRep a, DecodeRep b) => DecodeRep (Rep.Sum a b) where
  decodeRep j = Rep.Inl <$> decodeRep j <|> Rep.Inr <$> decodeRep j

instance decodeRepConstructor :: (IsSymbol name, DecodeRepArgs a) => DecodeRep (Rep.Constructor name a) where
  decodeRep j = do
    let name = reflectSymbol (SProxy :: SProxy name)
    let decodingErr msg = "When decoding a " <> name <> ": " <> msg
    jObj <- mFail (decodingErr "expected an object") (toObject j)
    jTag <- mFail (decodingErr "'tag' property is missing") (FO.lookup "tag" jObj)
    tag <- mFail (decodingErr "'tag' property is not a string") (toString jTag)
    when (tag /= name) $
      Left $ decodingErr "'tag' property has an incorrect value"
    jValues <- mFail (decodingErr "'values' property is missing") (FO.lookup "values" jObj)
    values <- mFail (decodingErr "'values' property is not an array") (toArray jValues)
    {init, rest} <- lmap decodingErr $ decodeRepArgs values
    when (rest /= []) $
      Left $ decodingErr "'values' property had too many values"
    pure $ Rep.Constructor init

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
    {head, tail} <- mFail "too few values were present" (uncons js)
    {init: _, rest: tail} <<< Rep.Argument <$> decodeJson head

-- | Decode `Json` representation of a value which has a `Generic` type.
genericDecodeJson :: forall a r. Rep.Generic a r => DecodeRep r => Json -> Either String a
genericDecodeJson = map Rep.to <<< decodeRep

mFail :: forall a. String -> Maybe a -> Either String a
mFail msg = maybe (Left msg) Right

-- | A function for decoding `Generic` sum types using string literal representations
decodeLiteralSum :: forall a r. Rep.Generic a r => DecodeLiteral r => Json -> Either String a
decodeLiteralSum = decodeLiteralSumWithTransform identity

-- | A function for decoding `Generic` sum types using string literal representations
-- | Takes a function for transforming the tag name in encoding
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
    tag <- mFail (decodingErr "could not read string for constructor") (toString j)
    when (tag /= tagNameTransform name) $
      Left $ decodingErr "string literal " <> tag <> " had an incorrect value."
    pure $ Rep.Constructor (Rep.NoArguments)


type FailMessage =
  Text "`decodeLiteralSum` can only be used with sum types, where all of the constructors are nullary. This is because a string literal cannot be encoded into a product type."

instance decodeLiteralConstructorCannotTakeProduct
  :: Fail FailMessage
  => DecodeLiteral (Rep.Product a b) where
    decodeLiteral _ _ = unsafeCrashWith "unreachable DecodeLiteral was reached."
