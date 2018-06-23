module LocalCooking.Common.ContentRecord where

import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Common.Tag.Culture (CultureTag)
import LocalCooking.Common.Tag.Diet (DietTag)
import LocalCooking.Common.Tag.Farm (FarmTag)
import LocalCooking.Common.Tag.Ingredient (IngredientTag)
import LocalCooking.Common.Tag.Meal (MealTag)

import Prelude
import Data.NonEmpty (NonEmpty (..))
import Data.Generic (class Generic, gEq, gCompare, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, fail, (~>), (:=), jsonEmptyObject, (.?))
import Control.Alternative ((<|>))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)



-- * Variants


data TagRecordVariant
  = TagVariantChef
  | TagVariantCulture
  | TagVariantDiet
  | TagVariantFarm
  | TagVariantIngredient
  | TagVariantMeal

derive instance genericTagRecordVariant :: Generic TagRecordVariant

instance eqTagRecordVariant :: Eq TagRecordVariant where
  eq = gEq

instance ordTagRecordVariant :: Ord TagRecordVariant where
  compare = gCompare

instance showTagRecordVariant :: Show TagRecordVariant where
  show = gShow

-- instance enumTagRecordVariant :: Enum TagRecordVariant where
-- instance boundedTagRecordVariant :: Bounded TagRecordVariant where

instance arbitraryTagRecordVariant :: Arbitrary TagRecordVariant where
  arbitrary = oneOf $ NonEmpty
    (pure TagVariantChef)
    [ pure TagVariantCulture
    , pure TagVariantDiet
    , pure TagVariantFarm
    , pure TagVariantIngredient
    , pure TagVariantMeal
    ]

instance encodeJsonTagRecordVariant :: EncodeJson TagRecordVariant where
  encodeJson x = encodeJson $ case x of
    TagVariantChef -> "chefTag"
    TagVariantCulture -> "cultureTag"
    TagVariantDiet -> "dietTag"
    TagVariantFarm -> "farmTag"
    TagVariantIngredient -> "ingredientTag"
    TagVariantMeal -> "mealTag"

instance decodeJsonTagRecordVariant :: DecodeJson TagRecordVariant where
  decodeJson json = do
    s <- decodeJson json
    case unit of
      _ | s == "chefTag" -> pure TagVariantChef
        | s == "cultureTag" -> pure TagVariantCulture
        | s == "dietTag" -> pure TagVariantDiet
        | s == "farmTag" -> pure TagVariantFarm
        | s == "ingredientTag" -> pure TagVariantIngredient
        | s == "mealTag" -> pure TagVariantMeal
        | otherwise -> fail "TagRecordVariant"


data ContentRecordVariant
  = TagRecordVariant TagRecordVariant

derive instance genericContentRecordVariant :: Generic ContentRecordVariant

instance eqContentRecordVariant :: Eq ContentRecordVariant where
  eq = gEq

instance ordContentRecordVariant :: Ord ContentRecordVariant where
  compare = gCompare

instance showContentRecordVariant :: Show ContentRecordVariant where
  show = gShow

-- instance enumContentRecordVariant :: Enum ContentRecordVariant where
-- instance boundedContentRecordVariant :: Bounded ContentRecordVariant where

instance arbitraryContentRecordVariant :: Arbitrary ContentRecordVariant where
  arbitrary = oneOf $ NonEmpty
    (TagRecordVariant <$> arbitrary)
    []

instance encodeJsonContentRecordVariant :: EncodeJson ContentRecordVariant where
  encodeJson x = case x of
    TagRecordVariant y -> "tagVariant" := y ~> jsonEmptyObject

instance decodeJsonContentRecordVariant :: DecodeJson ContentRecordVariant where
  decodeJson json = do
    o <- decodeJson json
    let tag = TagRecordVariant <$> o .? "tagVariant"
    tag



-- * Records


data TagRecord
  = TagRecordChef ChefTag
  | TagRecordCulture CultureTag
  | TagRecordDiet DietTag
  | TagRecordFarm FarmTag
  | TagRecordIngredient IngredientTag
  | TagRecordMeal MealTag

derive instance genericTagRecord :: Generic TagRecord

instance eqTagRecord :: Eq TagRecord where
  eq = gEq

instance showTagRecord :: Show TagRecord where
  show = gShow

instance arbitraryTagRecord :: Arbitrary TagRecord where
  arbitrary = oneOf $ NonEmpty
    (TagRecordChef <$> arbitrary)
    [ TagRecordCulture <$> arbitrary
    , TagRecordDiet <$> arbitrary
    , TagRecordFarm <$> arbitrary
    , TagRecordIngredient <$> arbitrary
    , TagRecordMeal <$> arbitrary
    ]

instance encodeJsonTagRecord :: EncodeJson TagRecord where
  encodeJson x = case x of
    TagRecordChef y -> "chef" := y ~> jsonEmptyObject
    TagRecordCulture y -> "culture" := y ~> jsonEmptyObject
    TagRecordDiet y -> "diet" := y ~> jsonEmptyObject
    TagRecordFarm y -> "farm" := y ~> jsonEmptyObject
    TagRecordIngredient y -> "ingredient" := y ~> jsonEmptyObject
    TagRecordMeal y -> "meal" := y ~> jsonEmptyObject

instance decodeJsonTagRecord :: DecodeJson TagRecord where
  decodeJson json = do
    o <- decodeJson json
    let chef = TagRecordChef <$> o .? "chef"
        culture = TagRecordCulture <$> o .? "culture"
        diet = TagRecordDiet <$> o .? "diet"
        farm = TagRecordFarm <$> o .? "farm"
        ingredient = TagRecordIngredient <$> o .? "ingredient"
        meal = TagRecordMeal <$> o .? "meal"
    chef <|> culture <|> diet <|> farm <|> ingredient <|> meal



data ContentRecord
  = TagRecord TagRecord

derive instance genericContentRecord :: Generic ContentRecord

instance eqContentRecord :: Eq ContentRecord where
  eq = gEq

instance showContentRecord :: Show ContentRecord where
  show = gShow

instance arbitraryContentRecord :: Arbitrary ContentRecord where
  arbitrary = oneOf $ NonEmpty
    (TagRecord <$> arbitrary)
    []

instance encodeJsonContentRecord :: EncodeJson ContentRecord where
  encodeJson x = case x of
    TagRecord y -> "tagRecord" := y ~> jsonEmptyObject

instance decodeJsonContentRecord :: DecodeJson ContentRecord where
  decodeJson json = do
    o <- decodeJson json
    let tag = TagRecord <$> o .? "tagRecord"
    tag
