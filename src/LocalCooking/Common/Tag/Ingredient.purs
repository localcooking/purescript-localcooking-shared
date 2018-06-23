module LocalCooking.Common.Tag.Ingredient where

import LocalCooking.Common.Tag (Tag)

import Prelude
import Data.Generic (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Newtype (class Newtype)
import Data.String.Yarn (class IsString)
import Test.QuickCheck (class Arbitrary)


newtype IngredientTag = IngredientTag Tag

derive instance genericIngredientTag :: Generic IngredientTag
derive instance newtypeIngredientTag :: Newtype IngredientTag _
derive newtype instance arbitraryIngredientTag :: Arbitrary IngredientTag
derive newtype instance eqIngredientTag :: Eq IngredientTag
derive newtype instance ordIngredientTag :: Ord IngredientTag
derive newtype instance showIngredientTag :: Show IngredientTag
derive newtype instance encodeJsonIngredientTag :: EncodeJson IngredientTag
derive newtype instance decodeJsonIngredientTag :: DecodeJson IngredientTag
derive newtype instance isStringIngredientTag :: IsString IngredientTag
