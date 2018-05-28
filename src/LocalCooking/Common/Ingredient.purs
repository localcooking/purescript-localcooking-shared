module LocalCooking.Common.Ingredient where

import LocalCooking.Common.Diet (Diet)

import Prelude
import Data.Generic (class Generic, gShow, gEq)
import Data.Argonaut (class EncodeJson, class DecodeJson, (~>), (:=), decodeJson, (.?), jsonEmptyObject)
import Test.QuickCheck (class Arbitrary, arbitrary)


newtype IngredientName = IngredientName String

derive instance genericIngredientName :: Generic IngredientName
derive newtype instance arbitraryIngredientName :: Arbitrary IngredientName
derive newtype instance eqIngredientName :: Eq IngredientName
derive newtype instance showIngredientName :: Show IngredientName
derive newtype instance encodeJsonIngredientName :: EncodeJson IngredientName
derive newtype instance decodeJsonIngredientName :: DecodeJson IngredientName


newtype Ingredient = Ingredient
  { name  :: IngredientName
  , voids :: Array Diet
  }

derive instance genericIngredient :: Generic Ingredient

instance arbitraryIngredient :: Arbitrary Ingredient where
  arbitrary = do
    name <- arbitrary
    voids <- arbitrary
    pure (Ingredient {name,voids})

instance eqIngredient :: Eq Ingredient where
  eq = gEq

instance showIngredient :: Show Ingredient where
  show = gShow

instance encodeJsonIngredient :: EncodeJson Ingredient where
  encodeJson (Ingredient {name,voids})
    =  "name" := name
    ~> "voids" := voids
    ~> jsonEmptyObject

instance decodeJsonIngredient :: DecodeJson Ingredient where
  decodeJson json = do
    o <- decodeJson json
    name <- o .? "name"
    voids <- o .? "voids"
    pure (Ingredient {name,voids})
