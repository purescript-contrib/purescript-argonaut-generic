module Data.Argonaut.Decode.Generic (
  gDecodeJson,
  gDecodeJson'
) where

import Prelude

import Data.Argonaut.Core (Json, toArray, toBoolean, toNumber, toObject, toString)
import Data.Array (zipWithA)
import Data.Either (Either(..))
import Data.Foldable (find)
import Data.Generic (class Generic, GenericSpine(..), GenericSignature(..), fromSpine, toSignature)
import Data.Int (fromNumber)
import Data.Maybe (maybe, Maybe(..))
import Data.String (toChar)
import Data.StrMap as SM
import Data.Traversable (traverse, for)
import Type.Proxy (Proxy(..))

-- | Decode `Json` representation of a value which has a `Generic` type.
gDecodeJson :: forall a. Generic a => Json -> Either String a
gDecodeJson
  = maybe (Left "fromSpine failed") Right
  <<< fromSpine
  <=< gDecodeJson' (toSignature (Proxy :: Proxy a))

-- | Decode `Json` representation of a `GenericSpine`.
gDecodeJson' :: GenericSignature -> Json -> Either String GenericSpine
gDecodeJson' signature json = case signature of
  SigNumber -> SNumber <$> mFail "Expected a number" (toNumber json)
  SigInt -> SInt <$> mFail "Expected an integer number" (fromNumber =<< toNumber json)
  SigString -> SString <$> mFail "Expected a string" (toString json)
  SigChar -> SChar <$> mFail "Expected a char" (toChar =<< toString json)
  SigBoolean -> SBoolean <$> mFail "Expected a boolean" (toBoolean json)
  SigUnit -> pure SUnit
  SigArray thunk -> do
    jArr <- mFail "Expected an array" $ toArray json
    SArray <$> traverse (map const <<< gDecodeJson' (thunk unit)) jArr
  SigRecord props -> do
    jObj <- mFail "Expected an object" $ toObject json
    SRecord <$> for props \({recLabel: lbl, recValue: val}) -> do
      pf <- mFail ("'" <> lbl <> "' property missing") (SM.lookup lbl jObj)
      sp <- gDecodeJson' (val unit) pf
      pure { recLabel: lbl, recValue: const sp }
  SigProd typeConstr alts -> do
    let decodingErr msg = "When decoding a " <> typeConstr <> ": " <> msg
    jObj <- mFail (decodingErr "expected an object") (toObject json)
    tagJson  <- mFail (decodingErr "'tag' property is missing") (SM.lookup "tag" jObj)
    tag <- mFail (decodingErr "'tag' property is not a string") (toString tagJson)
    case find ((tag == _) <<< _.sigConstructor) alts of
      Nothing -> Left (decodingErr ("'" <> tag <> "' isn't a valid constructor"))
      Just { sigValues: sigValues } -> do
        vals <- mFail (decodingErr "'values' array is missing") (toArray =<< SM.lookup "values" jObj)
        sps  <- zipWithA (\k -> gDecodeJson' (k unit)) sigValues vals
        pure (SProd tag (const <$> sps))
  where
  mFail :: forall a. String -> Maybe a -> Either String a
  mFail msg = maybe (Left msg) Right
