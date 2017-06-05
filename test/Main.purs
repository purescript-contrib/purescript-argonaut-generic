module Test.Main
  ( main
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Test.Assert (ASSERT, assert)

data Example
  = Either (Either String Example)
  | Record {foo :: Int, bar :: String}
  | Product Int Int Example

derive instance eqExample :: Eq Example
derive instance genericExample :: Generic Example _
instance showExample :: Show Example where
  show a = genericShow a
instance encodeJsonExample :: EncodeJson Example where
  encodeJson a = genericEncodeJson a
instance decodeJson :: DecodeJson Example where
  decodeJson a = genericDecodeJson a

main :: forall eff. Eff (assert :: ASSERT, console :: CONSOLE | eff) Unit
main = do
  example $ Either $ Left "foo"
  example $ Either $ Right $ Either $ Left "foo"
  example $ Record {foo: 42, bar: "bar"}
  example $ Product 1 2 $ Either $ Left "foo"
  where
  example original = do
    let json = encodeJson original
    let parsed = decodeJson json
    log $ "Original:  " <> show original
    log $ "To JSON:   " <> stringify json
    log $ "From JSON: " <> show parsed
    assert $ parsed == Right original
    log $ "--------------------------------------------------------------------------------"
