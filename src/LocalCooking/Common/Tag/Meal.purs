module LocalCooking.Common.Tag.Meal where

import LocalCooking.Common.Tag (Tag)

import Prelude
import Data.Generic (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Newtype (class Newtype)
import Test.QuickCheck (class Arbitrary)


newtype MealTag = MealTag Tag

derive instance genericMealTag :: Generic MealTag
derive instance newtypeMealTag :: Newtype MealTag _
derive newtype instance arbitraryMealTag :: Arbitrary MealTag
derive newtype instance eqMealTag :: Eq MealTag
derive newtype instance ordMealTag :: Ord MealTag
derive newtype instance showMealTag :: Show MealTag
derive newtype instance encodeJsonMealTag :: EncodeJson MealTag
derive newtype instance decodeJsonMealTag :: DecodeJson MealTag
