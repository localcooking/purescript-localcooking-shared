module LocalCooking.Semantics.ContentRecord where

import LocalCooking.Semantics.Chef (GetSetChef, MenuSettings, MealSettings)
import LocalCooking.Semantics.Common (WithId)
import LocalCooking.Database.Schema (StoredMenuId, StoredMealId)
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Common.Tag.Culture (CultureTag)
import LocalCooking.Common.Tag.Diet (DietTag)
import LocalCooking.Common.Tag.Farm (FarmTag)
import LocalCooking.Common.Tag.Ingredient (IngredientTag)
import LocalCooking.Common.Tag.Meal (MealTag)

import Prelude
import Data.Maybe (Maybe (..))
import Data.NonEmpty (NonEmpty (..))
import Data.Generic (class Generic, gEq, gCompare, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, fail, (~>), (:=), jsonEmptyObject, (.?))
import Data.Enum (class Enum, class BoundedEnum, Cardinality (..), cardinality, pred, succ, toEnum, fromEnum)
import Data.Bounded (class Bounded, top, bottom)
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

instance enumTagRecordVariant :: Enum TagRecordVariant where
  succ x = case x of
    TagVariantChef -> Just TagVariantCulture
    TagVariantCulture -> Just TagVariantDiet
    TagVariantDiet -> Just TagVariantFarm
    TagVariantFarm -> Just TagVariantIngredient
    TagVariantIngredient -> Just TagVariantMeal
    TagVariantMeal -> Nothing
  pred x = case x of
    TagVariantChef -> Nothing
    TagVariantCulture -> Just TagVariantChef
    TagVariantDiet -> Just TagVariantCulture
    TagVariantFarm -> Just TagVariantDiet
    TagVariantIngredient -> Just TagVariantFarm
    TagVariantMeal -> Just TagVariantIngredient

instance boundedTagRecordVariant :: Bounded TagRecordVariant where
  top = TagVariantMeal
  bottom = TagVariantChef

instance boundedEnumTagRecordVariant :: BoundedEnum TagRecordVariant where
  cardinality = Cardinality 6
  toEnum n
    | n == 0 = Just TagVariantChef
    | n == 1 = Just TagVariantCulture
    | n == 2 = Just TagVariantDiet
    | n == 3 = Just TagVariantFarm
    | n == 4 = Just TagVariantIngredient
    | n == 5 = Just TagVariantMeal
    | otherwise = Nothing
  fromEnum x = case x of
    TagVariantChef -> 0
    TagVariantCulture -> 1
    TagVariantDiet -> 2
    TagVariantFarm -> 3
    TagVariantIngredient -> 4
    TagVariantMeal -> 5

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



data ChefRecordVariant
  = ChefVariantChef
  | ChefVariantMenu
  | ChefVariantMeal

derive instance genericChefRecordVariant :: Generic ChefRecordVariant

instance eqChefRecordVariant :: Eq ChefRecordVariant where
  eq = gEq

instance ordChefRecordVariant :: Ord ChefRecordVariant where
  compare = gCompare

instance showChefRecordVariant :: Show ChefRecordVariant where
  show = gShow

instance enumChefRecordVariant :: Enum ChefRecordVariant where
  succ x = case x of
    ChefVariantChef -> Just ChefVariantMenu
    ChefVariantMenu -> Just ChefVariantMeal
    ChefVariantMeal -> Nothing
  pred x = case x of
    ChefVariantChef -> Nothing
    ChefVariantMenu -> Just ChefVariantChef
    ChefVariantMeal -> Just ChefVariantMenu

instance boundedChefRecordVariant :: Bounded ChefRecordVariant where
  top = ChefVariantMeal
  bottom = ChefVariantChef

instance boundedEnumChefRecordVariant :: BoundedEnum ChefRecordVariant where
  cardinality = Cardinality 3
  toEnum n
    | n == 0 = Just ChefVariantChef
    | n == 1 = Just ChefVariantMenu
    | n == 2 = Just ChefVariantMeal
    | otherwise = Nothing
  fromEnum x = case x of
    ChefVariantChef -> 0
    ChefVariantMenu -> 1
    ChefVariantMeal -> 2

instance arbitraryChefRecordVariant :: Arbitrary ChefRecordVariant where
  arbitrary = oneOf $ NonEmpty
    (pure ChefVariantChef)
    [ pure ChefVariantMenu
    , pure ChefVariantMeal
    ]

instance encodeJsonChefRecordVariant :: EncodeJson ChefRecordVariant where
  encodeJson x = encodeJson $ case x of
    ChefVariantChef -> "chefChef"
    ChefVariantMenu -> "menuChef"
    ChefVariantMeal -> "mealChef"

instance decodeJsonChefRecordVariant :: DecodeJson ChefRecordVariant where
  decodeJson json = do
    s <- decodeJson json
    case unit of
      _ | s == "chefChef" -> pure ChefVariantChef
        | s == "menuChef" -> pure ChefVariantMenu
        | s == "mealChef" -> pure ChefVariantMeal
        | otherwise -> fail "ChefRecordVariant"




data ContentRecordVariant
  = TagRecordVariant TagRecordVariant
  | ChefRecordVariant ChefRecordVariant

derive instance genericContentRecordVariant :: Generic ContentRecordVariant

instance eqContentRecordVariant :: Eq ContentRecordVariant where
  eq = gEq

instance ordContentRecordVariant :: Ord ContentRecordVariant where
  compare = gCompare

instance showContentRecordVariant :: Show ContentRecordVariant where
  show = gShow

instance enumContentRecordVariant :: Enum ContentRecordVariant where
  succ x = case x of
    TagRecordVariant y -> case succ y of
      Just z -> Just (TagRecordVariant z)
      Nothing -> Just (ChefRecordVariant bottom)
    ChefRecordVariant y -> case succ y of
      Just z -> Just (ChefRecordVariant z)
      Nothing -> Nothing
  pred x = case x of
    TagRecordVariant y -> case pred y of
      Nothing -> Nothing
      Just z -> Just (TagRecordVariant z)
    ChefRecordVariant y -> case pred y of
      Nothing -> Just (TagRecordVariant top)
      Just z -> Just (ChefRecordVariant z)

instance boundedContentRecordVariant :: Bounded ContentRecordVariant where
  top = ChefRecordVariant top
  bottom = TagRecordVariant bottom

instance boundedEnumContentRecordVariant :: BoundedEnum ContentRecordVariant where
  cardinality = Cardinality $
      unCardinality (cardinality :: Cardinality TagRecordVariant)
    + unCardinality (cardinality :: Cardinality ChefRecordVariant)
  toEnum n = case unit of
    _ | n >= 0
        && n < unCardinality
              ( cardinality :: Cardinality TagRecordVariant
              ) -> TagRecordVariant <$> toEnum n
      | n >= unCardinality (cardinality :: Cardinality TagRecordVariant)
        && n < ( unCardinality (cardinality :: Cardinality TagRecordVariant)
               + unCardinality (cardinality :: Cardinality ChefRecordVariant)
               ) -> ChefRecordVariant <$> toEnum
                     (n - unCardinality (cardinality :: Cardinality TagRecordVariant))
      | otherwise -> Nothing
  fromEnum x = case x of
    TagRecordVariant y -> fromEnum y
    ChefRecordVariant y -> fromEnum y
                         + unCardinality (cardinality :: Cardinality TagRecordVariant)

unCardinality :: forall a. Cardinality a -> Int
unCardinality (Cardinality x) = x


instance arbitraryContentRecordVariant :: Arbitrary ContentRecordVariant where
  arbitrary = oneOf $ NonEmpty
    (TagRecordVariant <$> arbitrary)
    [ChefRecordVariant <$> arbitrary]

instance encodeJsonContentRecordVariant :: EncodeJson ContentRecordVariant where
  encodeJson x = case x of
    TagRecordVariant y -> "tagVariant" := y ~> jsonEmptyObject
    ChefRecordVariant y -> "chefVariant" := y ~> jsonEmptyObject

instance decodeJsonContentRecordVariant :: DecodeJson ContentRecordVariant where
  decodeJson json = do
    o <- decodeJson json
    let tag = TagRecordVariant <$> o .? "tagVariant"
        chef = ChefRecordVariant <$> o .? "chefVariant"
    tag <|> chef



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



data ChefRecord
  = ChefRecordChef GetSetChef
  | ChefRecordSetMenu (WithId StoredMenuId MenuSettings)
  | ChefRecordNewMenu MenuSettings
  | ChefRecordSetMeal (WithId StoredMenuId (WithId StoredMealId MealSettings))
  | ChefRecordNewMeal (WithId StoredMenuId MealSettings)

derive instance genericChefRecord :: Generic ChefRecord

instance eqChefRecord :: Eq ChefRecord where
  eq = gEq

instance showChefRecord :: Show ChefRecord where
  show = gShow

instance arbitraryChefRecord :: Arbitrary ChefRecord where
  arbitrary = oneOf $ NonEmpty
    (ChefRecordChef <$> arbitrary)
    [ ChefRecordSetMenu <$> arbitrary
    , ChefRecordNewMenu <$> arbitrary
    , ChefRecordSetMeal <$> arbitrary
    , ChefRecordNewMeal <$> arbitrary
    ]

instance encodeJsonChefRecord :: EncodeJson ChefRecord where
  encodeJson x = case x of
    ChefRecordChef y -> "chef" := y ~> jsonEmptyObject
    ChefRecordSetMenu y -> "setMenu" := y ~> jsonEmptyObject
    ChefRecordNewMenu y -> "newMenu" := y ~> jsonEmptyObject
    ChefRecordSetMeal y -> "setMeal" := y ~> jsonEmptyObject
    ChefRecordNewMeal y -> "newMeal" := y ~> jsonEmptyObject

instance decodeJsonChefRecord :: DecodeJson ChefRecord where
  decodeJson json = do
    o <- decodeJson json
    let chef = ChefRecordChef <$> o .? "chef"
        setMenu = ChefRecordSetMenu <$> o .? "setMenu"
        newMenu = ChefRecordNewMenu <$> o .? "newMenu"
        setMeal = ChefRecordSetMeal <$> o .? "setMeal"
        newMeal = ChefRecordNewMeal <$> o .? "newMeal"
    chef <|> setMenu <|> newMenu <|> setMeal <|> newMeal



data ContentRecord
  = TagRecord TagRecord
  | ChefRecord ChefRecord

derive instance genericContentRecord :: Generic ContentRecord

instance eqContentRecord :: Eq ContentRecord where
  eq = gEq

instance showContentRecord :: Show ContentRecord where
  show = gShow

instance arbitraryContentRecord :: Arbitrary ContentRecord where
  arbitrary = oneOf $ NonEmpty
    (TagRecord <$> arbitrary)
    [ChefRecord <$> arbitrary]

instance encodeJsonContentRecord :: EncodeJson ContentRecord where
  encodeJson x = case x of
    TagRecord y -> "tagRecord" := y ~> jsonEmptyObject
    ChefRecord y -> "chefRecord" := y ~> jsonEmptyObject

instance decodeJsonContentRecord :: DecodeJson ContentRecord where
  decodeJson json = do
    o <- decodeJson json
    let tag = TagRecord <$> o .? "tagRecord"
        chef = ChefRecord <$> o .? "chefRecord"
    tag <|> chef
