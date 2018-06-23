module LocalCooking.Common.Ingredient where

import LocalCooking.Common.Tag.Diet (DietTag)
import LocalCooking.Common.Tag.Ingredient (IngredientTag)

import Prelude
import Data.Generic (class Generic, gShow, gEq)
import Data.Argonaut (class EncodeJson, class DecodeJson, (~>), (:=), decodeJson, (.?), jsonEmptyObject)
import Data.String.Yarn (class IsString)
import Test.QuickCheck (class Arbitrary, arbitrary)



newtype Ingredient = Ingredient
  { name  :: IngredientTag
  , voids :: Array DietTag
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
