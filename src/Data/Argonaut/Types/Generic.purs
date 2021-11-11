module Data.Argonaut.Types.Generic
  ( Encoding(..)
  , defaultEncoding
  ) where

-- | Encoding settings:
-- | tagKey -- which key to use in the JSON object for sum-type constructors
-- | valuesKey -- which key to use in the JSON object for sum-type values
-- | unwrapSingleArguments -- should single-arguments constructors' values be wrapped in an array
type Encoding =
  { tagKey :: String
  , valuesKey :: String
  , unwrapSingleArguments :: Boolean
  }

defaultEncoding :: Encoding
defaultEncoding =
  { tagKey: "tag"
  , valuesKey: "values"
  , unwrapSingleArguments: false
  }
