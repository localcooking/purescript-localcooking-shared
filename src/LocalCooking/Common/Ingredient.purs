module LocalCooking.Common.Ingredient where

import LocalCooking.Common.Diet (Diet)

import Prelude
import Data.Generic (class Generic, gShow, gEq)
import Data.Argonaut (class EncodeJson, class DecodeJson, (~>), (:=), decodeJson, (.?), jsonEmptyObject)


newtype Ingredient = Ingredient
  { name  :: String
  , voids :: Array Diet
  }

derive instance genericIngredient :: Generic Ingredient

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
