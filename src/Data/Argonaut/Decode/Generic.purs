module Data.Argonaut.Decode.Generic (
  gDecodeJson,
  gDecodeJson',

  class DecodeRep,
  class DecodeRepArgs,
  class DecodeRepFields,
  decodeRep,
  decodeRepArgs,
  decodeRepFields,
  genericDecodeJson
) where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut.Core (Json, toArray, toBoolean, toNumber, toObject, toString)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Array (uncons, zipWithA)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (find)
import Data.Generic (class Generic, GenericSpine(..), GenericSignature(..), fromSpine, toSignature)
import Data.Generic.Rep as Rep
import Data.Int (fromNumber)
import Data.Maybe (maybe, Maybe(..))
import Data.String (toChar)
import Data.StrMap as SM
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
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
    jTag <- mFail (decodingErr "'tag' property is missing") (SM.lookup "tag" jObj)
    tag <- mFail (decodingErr "'tag' property is not a string") (toString jTag)
    when (tag /= name) $
      Left $ decodingErr "'tag' property has an incorrect value"
    jValues <- mFail (decodingErr "'values' property is missing") (SM.lookup "values" jObj)
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

instance decodeRepArgsRec :: (DecodeRepFields fields) => DecodeRepArgs (Rep.Rec fields) where
  decodeRepArgs js = do
    {head, tail} <- mFail "too few values were present" (uncons js)
    jObj <- mFail "record is not encoded as an object" (toObject head)
    {init: _, rest: tail} <<< Rep.Rec <$> decodeRepFields jObj

class DecodeRepFields r where
  decodeRepFields :: SM.StrMap Json -> Either String r

instance decodeRepFieldsProduct :: (DecodeRepFields a, DecodeRepFields b) => DecodeRepFields (Rep.Product a b) where
  decodeRepFields js = Rep.Product <$> decodeRepFields js <*> decodeRepFields js

instance decodeRepFieldsField :: (IsSymbol field, DecodeJson a) => DecodeRepFields (Rep.Field field a) where
  decodeRepFields js = do
    let name = reflectSymbol (SProxy :: SProxy field)
    value <- mFail ("the field '" <> name <> "' is not present") (SM.lookup name js)
    Rep.Field <$> decodeJson value

genericDecodeJson :: forall a r. Rep.Generic a r => DecodeRep r => Json -> Either String a
genericDecodeJson = map Rep.to <<< decodeRep

mFail :: forall a. String -> Maybe a -> Either String a
mFail msg = maybe (Left msg) Right
