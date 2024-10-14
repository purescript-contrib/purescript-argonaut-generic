module Test.Main
  ( main
  ) where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Generic (class DecodeLiteral, decodeLiteralSumWithTransform, genericDecodeJson, genericDecodeJsonWith)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic (class EncodeLiteral, encodeLiteralSumWithTransform, genericEncodeJson, genericEncodeJsonWith)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Types.Generic (Encoding, defaultEncoding)
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String (toLower, toUpper)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Partial.Unsafe (unsafeCrashWith)
import Test.Assert (assert)

data Example
  = Either (Either String Example)
  | Record { foo :: Int, bar :: String }
  | Nested { foo :: { nested :: Int }, bar :: String }
  | Product Int Int Example

derive instance eqExample :: Eq Example
derive instance genericExample :: Generic Example _
instance showExample :: Show Example where
  show a = genericShow a

instance encodeJsonExample :: EncodeJson Example where
  encodeJson a = genericEncodeJson a

instance decodeJson :: DecodeJson Example where
  decodeJson a = genericDecodeJson a

data LiteralStringExample
  = Apple
  | Banana
  | Frikandel

derive instance eqLiteralStringExample :: Eq LiteralStringExample
derive instance genericLiteralStringExample :: Generic LiteralStringExample _
instance showLiteralStringExample :: Show LiteralStringExample where
  show a = genericShow a

instance encodeJsonLiteralStringExample :: EncodeJson LiteralStringExample where
  encodeJson a = encodeLiteralSumWithTransform identity a

instance decodeJsonLiteralStringExample :: DecodeJson LiteralStringExample where
  decodeJson a = decodeLiteralSumWithTransform identity a

diffEncodingOptions :: Encoding
diffEncodingOptions = defaultEncoding
  { tagKey = "type"
  , valuesKey = "value"
  }

data DiffEncoding = A | B Int

derive instance eqDiffEncoding :: Eq DiffEncoding
derive instance genericDiffEncoding :: Generic DiffEncoding _
instance showDiffENcoding :: Show DiffEncoding where
  show a = genericShow a

instance encodeJsonDiffEncoding :: EncodeJson DiffEncoding where
  encodeJson a = genericEncodeJsonWith diffEncodingOptions a

instance decodeJsonDiffEncoding :: DecodeJson DiffEncoding where
  decodeJson a = genericDecodeJsonWith diffEncodingOptions a

unwrapSingleArgsOptions :: Encoding
unwrapSingleArgsOptions = defaultEncoding
  { unwrapSingleArguments = true
  }

data UnwrapSingleArgs = U0 Int | U1 Int Int

derive instance eqUnwrapSingleArgs :: Eq UnwrapSingleArgs
derive instance genericUnwrapSingleArgs :: Generic UnwrapSingleArgs _
instance showUnwrapSingleArgs :: Show UnwrapSingleArgs where
  show a = genericShow a

instance encodeJsonUnwrapSingleArgs :: EncodeJson UnwrapSingleArgs where
  encodeJson a = genericEncodeJsonWith unwrapSingleArgsOptions a

instance decodeJsonUnwrapSingleArgs :: DecodeJson UnwrapSingleArgs where
  decodeJson a = genericDecodeJsonWith unwrapSingleArgsOptions a

data IgnoreNullaryArgs = NA0 | NA1 Int

derive instance eqIgnoreNullaryArgs :: Eq IgnoreNullaryArgs
derive instance genericIgnoreNullaryArgs :: Generic IgnoreNullaryArgs _
instance showIgnoreNullaryArgs :: Show IgnoreNullaryArgs where
  show a = genericShow a

instance encodeJsonIgnoreNullaryArgs :: EncodeJson IgnoreNullaryArgs where
  encodeJson a = genericEncodeJson a

instance decodeJsonIgnoreNullaryArgs :: DecodeJson IgnoreNullaryArgs where
  decodeJson a = genericDecodeJson a

jsonParser' :: String -> Effect Json
jsonParser' = either throw pure <<< jsonParser

main :: Effect Unit
main = do
  example $ Either $ Left "foo"
  example $ Either $ Right $ Either $ Left "foo"
  example $ Record { foo: 42, bar: "bar" }
  example $ Nested { foo: { nested: 42 }, bar: "bar" }
  example $ Product 1 2 $ Either $ Left "foo"
  example $ Frikandel
  example $ A
  example $ B 42

  example $ U0 42
  assert $ stringify (encodeJson (U0 42)) == """{"values":42,"tag":"U0"}"""

  example $ U1 1 2
  assert $ stringify (encodeJson (U1 1 2)) == """{"values":[1,2],"tag":"U1"}"""

  testLiteralSumWithTransform identity Frikandel "\"Frikandel\""
  testLiteralSumWithTransform toUpper Frikandel "\"FRIKANDEL\""
  testLiteralSumWithTransform toLower Frikandel "\"frikandel\""

  example $ NA1 42
  example $ NA0
  json <- jsonParser' """{"tag":"NA0"}"""
  assert $ (decodeJson json) == Right NA0

  where
  example :: forall a. Show a => Eq a => EncodeJson a => DecodeJson a => a -> Effect Unit
  example original = do
    let json = encodeJson original
    let parsed = decodeJson json
    log $ "Original:  " <> show original
    log $ "To JSON:   " <> stringify json
    log $ "From JSON: " <> show parsed
    assert $ parsed == Right original
    log $ "--------------------------------------------------------------------------------"

  testLiteralSumWithTransform
    :: forall a rep
     . Show a
    => Eq a
    => Generic a rep
    => EncodeLiteral rep
    => DecodeLiteral rep
    => (String -> String)
    -> a
    -> String
    -> Effect Unit
  testLiteralSumWithTransform tagNameTransform original string = do
    let json = encodeLiteralSumWithTransform tagNameTransform original
    let parsed = decodeLiteralSumWithTransform tagNameTransform json
    let parsed' = decodeLiteralSumWithTransform tagNameTransform <<< either unsafeCrashWith identity $ jsonParser string
    log $ "Original:  " <> show original
    log $ "To JSON:   " <> stringify json
    log $ "From JSON: " <> show parsed
    assert $ parsed == Right original
    assert $ parsed' == Right original
    log $ "--------------------------------------------------------------------------------"
