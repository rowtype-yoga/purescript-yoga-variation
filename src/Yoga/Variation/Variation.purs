module Yoga.Variation.Variation
  ( Validated
  , invalid
  , key
  , valid
  )
  where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmpty
import Data.Symbol (class IsSymbol)
import Data.Validation.Semigroup as Validation
import Data.Variant as Variant
import Prim.Row (class Cons)
import Type.Proxy (Proxy(..))

type Validated err result = Validation.V (NonEmptyArray (Variant.Variant err)) result

valid :: forall err result. result -> Validated err result
valid = pure

invalid
  ∷ ∀ (t63 ∷ Type) (sym ∷ Symbol) (error ∷ Type) (others ∷ Row Type) (errors ∷ Row Type)
  . Cons sym error others errors
  ⇒ IsSymbol sym
  ⇒ Proxy sym
  → error
  → Validation.V (NonEmptyArray (Variant.Variant errors)) t63
invalid proxy value = Validation.invalid $ NonEmpty.singleton $ Variant.inj proxy value

key ∷ ∀ (sym ∷ Symbol). Proxy sym
key = Proxy
