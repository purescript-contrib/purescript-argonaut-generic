module Data.Argonaut.Encode.Generic.Rep (
  class EncodeRep,
  class EncodeRepArgs,
  class EncodeRepFields,
  class EncodeRepRowList,
  class EncodeLiteral,
  encodeRep,
  encodeRepArgs,
  encodeRepFields,
  encodeRepRowList,
  genericEncodeJson,
  encodeLiteralSum,
  encodeLiteralSumWithTransform,
  encodeLiteral
) where

import Prelude

import Data.Argonaut.Core (Json, fromArray, fromObject, fromString)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Generic.Rep as Rep
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Foreign.Object as FO
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Prim.TypeError (class Fail, Text)
import Record (get)
import Type.Data.RowList (RLProxy(..))

class EncodeRep r where
  encodeRep :: r -> Json

instance encodeRepNoConstructors :: EncodeRep Rep.NoConstructors where
  encodeRep r = encodeRep r

instance encodeRepSum :: (EncodeRep a, EncodeRep b) => EncodeRep (Rep.Sum a b) where
  encodeRep (Rep.Inl a) = encodeRep a
  encodeRep (Rep.Inr b) = encodeRep b

instance encodeRepConstructor :: (IsSymbol name, EncodeRepArgs a) => EncodeRep (Rep.Constructor name a) where
  encodeRep (Rep.Constructor a) =
    fromObject
      $ FO.insert "tag" (fromString (reflectSymbol (SProxy :: SProxy name)))
      $ FO.insert "values" (fromArray (encodeRepArgs a))
      $ FO.empty

class EncodeRepArgs r where
  encodeRepArgs :: r -> Array Json

instance encodeRepArgsNoArguments :: EncodeRepArgs Rep.NoArguments where
  encodeRepArgs Rep.NoArguments = []

instance encodeRepArgsProduct :: (EncodeRepArgs a, EncodeRepArgs b) => EncodeRepArgs (Rep.Product a b) where
  encodeRepArgs (Rep.Product a b) = encodeRepArgs a <> encodeRepArgs b

instance encodeRepRecordArgument :: (RowToList row rl, EncodeRepRowList rl row) => EncodeRepArgs (Rep.Argument (Record row)) where
  encodeRepArgs (Rep.Argument rec) = [ fromObject (encodeRepRowList rlp rec FO.empty) ]
    where rlp = RLProxy :: RLProxy rl

else instance encodeRepArgsArgument :: (EncodeJson a) => EncodeRepArgs (Rep.Argument a) where
  encodeRepArgs (Rep.Argument a) = [encodeJson a]

class EncodeRepFields r where
  encodeRepFields :: r -> FO.Object Json

instance encodeRepFieldsProduct :: (EncodeRepFields a, EncodeRepFields b) => EncodeRepFields (Rep.Product a b) where
  encodeRepFields (Rep.Product a b) =
    FO.union (encodeRepFields a) (encodeRepFields b)


-- | a `EncodeRepRowList` represents a relation between a `RowList` and a record you
-- | can serialize into a Json `Object`
-- |
-- | this one is strictly internal to help out `encodeRepRecordArgument` handling records 
-- |
-- | a `RowList` on the type level is very similar to a *cons-list* on the value level
-- | so the two instances handle all possible `RowList`s
-- |
-- | the idea is use the `Cons` cases to to compose functions that adds the field
-- | and values from the given record into a Json-`Object`
-- | the field in question is indicated by the head of the `RowList`
-- |
-- | the `Nil` case just returns `identity` to bootstrap the composition-chain
class EncodeRepRowList (rl :: RowList) (row :: #Type) | rl -> row where
  encodeRepRowList :: forall g . g rl -> Record row -> (FO.Object Json -> FO.Object Json)

instance encodeRepRowListNil :: EncodeRepRowList Nil row where
  encodeRepRowList _ _ = identity

instance encodeRepRowListCons :: (EncodeJson ty, IsSymbol name, EncodeRepRowList tail row, Row.Cons name ty ignore row) => EncodeRepRowList (Cons name ty tail) row where
  encodeRepRowList _ rec = \obj -> FO.insert (reflectSymbol namep) (encodeJson value) (cont obj)
    where
      namep = SProxy :: SProxy name
      value = get namep rec
      tailp = RLProxy :: RLProxy tail
      cont  = encodeRepRowList tailp rec



-- | Encode any `Generic` data structure into `Json`.
genericEncodeJson :: forall a r. Rep.Generic a r => EncodeRep r => a -> Json
genericEncodeJson = encodeRep <<< Rep.from

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
