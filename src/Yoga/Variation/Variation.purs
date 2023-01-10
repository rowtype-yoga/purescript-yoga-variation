module Yoga.Variation.Variation
  ( Validated
  , fromEither
  , invalid
  , invalidM
  , key
  , mapErrors
  , module Validation
  , module Variant
  , valid
  , validM
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmpty
import Data.Bifunctor (class Bifunctor, lmap)
import Data.Either (Either, either)
import Data.Symbol (class IsSymbol)
import Data.Validation.Semigroup as ValidationSG
import Data.Validation.Semigroup hiding (invalid) as Validation
import Data.Variant (class Contractable, class VariantBounded, class VariantBoundedEnums, class VariantEqs, class VariantMapCases, class VariantMatchCases, class VariantOrds, class VariantShows, class VariantTraverseCases, Unvariant(..), Unvariant', Variant, case_, contract, default, expand, inj, match, on, onMatch, over, overOne, overSome, prj, revariant, traverse, traverseOne, traverseSome, unvariant, variantBounded, variantBoundedEnums, variantEqs, variantOrds, variantShows) as Variant
import Prim.Row (class Cons, class Union)
import Prim.RowList (class RowToList)
import Type.Proxy (Proxy(..))

type Validated err result = Validation.V (NonEmptyArray (Variant.Variant err)) result

valid :: forall err result. result -> Validated err result
valid = pure

validM :: forall m err result. Applicative m => result -> m (Validated err result)
validM = valid >>> pure

invalid
  ∷ ∀ (result ∷ Type) (sym ∷ Symbol) (error ∷ Type) (others ∷ Row Type) (errors ∷ Row Type)
  . Cons sym error others errors
  ⇒ IsSymbol sym
  ⇒ Proxy sym
  → error
  → Validation.V (NonEmptyArray (Variant.Variant errors)) result
invalid proxy value = ValidationSG.invalid $ NonEmpty.singleton $ Variant.inj proxy value

invalidM
  ∷ ∀ (result ∷ Type) (sym ∷ Symbol) (error ∷ Type) (others ∷ Row Type) (errors ∷ Row Type) m
   . Applicative m
  => Cons sym error others errors
  ⇒ IsSymbol sym
  ⇒ Proxy sym
  → error
  → m (Validation.V (NonEmptyArray (Variant.Variant errors)) result)
invalidM proxy = invalid proxy >>> pure

fromEither
  :: ∀ (result ∷ Type) (sym ∷ Symbol) (error ∷ Type) (others ∷ Row Type) (errors ∷ Row Type)
   . Cons sym error others errors
  ⇒ IsSymbol sym
  ⇒ Proxy sym
  -> Either error result
  -> Validated errors result
fromEither proxy = either (invalid proxy) valid

key ∷ ∀ (sym ∷ Symbol). Proxy sym
key = Proxy

mapErrors
  :: forall result handlersRl handlers errorsRemaining errorsOut errorsIn
   . RowToList handlers handlersRl
  => Variant.VariantMatchCases handlersRl errorsRemaining (Variant.Variant errorsOut)
  => Union errorsRemaining errorsOut errorsIn
  => Record handlers
  -> Validated errorsIn result
  -> Validated errorsOut result
mapErrors handlers = lmap (map $ Variant.onMatch handlers identity)
