module Data.Argonaut.Types.Generic.Rep (
  Encoding(..),
  defaultEncoding
) where

type Encoding =
  { tagKey :: String
  , valuesKey :: String
  }

defaultEncoding :: Encoding
defaultEncoding =
  { tagKey: "tag"
  , valuesKey: "values"
  }

