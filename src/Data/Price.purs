module Data.Price where

import Prelude
import Data.NonEmpty (NonEmpty (..))
import Data.Generic (class Generic, gEq, gCompare, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, fail, (:=), (~>), jsonEmptyObject, (.?))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)



data Currency
  = USD

derive instance genericCurrency :: Generic Currency

instance arbitraryCurrency :: Arbitrary Currency where
  arbitrary = oneOf $ NonEmpty
    ( pure USD
    )
    []

instance eqCurrency :: Eq Currency where
  eq = gEq

instance ordCurrency :: Ord Currency where
  compare = gCompare

instance showCurrency :: Show Currency where
  show x = case x of
    USD -> "usd"

instance encodeJsonCurrency :: EncodeJson Currency where
  encodeJson = encodeJson <<< show

instance decodeJsonCurrency :: DecodeJson Currency where
  decodeJson json = do
    s <- decodeJson json
    case unit of
      _ | s == "usd" -> pure USD
        | otherwise -> fail "Not a Currency"


newtype Price = Price
  { currency :: Currency
  , amount   :: Number
  }

derive instance genericPrice :: Generic Price

instance arbitraryPrice :: Arbitrary Price where
  arbitrary = do
    currency <- arbitrary
    amount <- arbitrary
    pure (Price {currency,amount})

instance eqPrice :: Eq Price where
  eq = gEq

instance showPrice :: Show Price where
  show = gShow

instance encodeJsonPrice :: EncodeJson Price where
  encodeJson (Price {currency,amount})
    =  "currency" := currency
    ~> "amount" := amount
    ~> jsonEmptyObject

instance decodeJsonPrice :: DecodeJson Price where
  decodeJson json = do
    o <- decodeJson json
    currency <- o .? "currency"
    amount <- o .? "amount"
    pure (Price {currency,amount})
