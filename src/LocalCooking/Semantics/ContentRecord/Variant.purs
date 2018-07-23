module LocalCooking.Semantics.ContentRecord.Variant where

import LocalCooking.Semantics.Chef (SetChef, MenuSettings, MealSettings)
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
  = ChefVariantMenu
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
    ChefVariantMenu -> Just ChefVariantMeal
    ChefVariantMeal -> Nothing
  pred x = case x of
    ChefVariantMenu -> Nothing
    ChefVariantMeal -> Just ChefVariantMenu

instance boundedChefRecordVariant :: Bounded ChefRecordVariant where
  top = ChefVariantMeal
  bottom = ChefVariantMenu

instance boundedEnumChefRecordVariant :: BoundedEnum ChefRecordVariant where
  cardinality = Cardinality 3
  toEnum n
    | n == 0 = Just ChefVariantMenu
    | n == 1 = Just ChefVariantMeal
    | otherwise = Nothing
  fromEnum x = case x of
    ChefVariantMenu -> 0
    ChefVariantMeal -> 1

instance arbitraryChefRecordVariant :: Arbitrary ChefRecordVariant where
  arbitrary = oneOf $ NonEmpty
    (pure ChefVariantMenu)
    [ pure ChefVariantMeal
    ]

instance encodeJsonChefRecordVariant :: EncodeJson ChefRecordVariant where
  encodeJson x = encodeJson $ case x of
    ChefVariantMenu -> "menuChef"
    ChefVariantMeal -> "mealChef"

instance decodeJsonChefRecordVariant :: DecodeJson ChefRecordVariant where
  decodeJson json = do
    s <- decodeJson json
    case unit of
      _ | s == "menuChef" -> pure ChefVariantMenu
        | s == "mealChef" -> pure ChefVariantMeal
        | otherwise -> fail "ChefRecordVariant"


data ProfileRecordVariant
  = ProfileVariantChef
  | ProfileVariantCustomer
  | ProfileVariantEditor
  | ProfileVariantFarmer
  | ProfileVariantRestaurant

derive instance genericProfileRecordVariant :: Generic ProfileRecordVariant

instance eqProfileRecordVariant :: Eq ProfileRecordVariant where
  eq = gEq

instance ordProfileRecordVariant :: Ord ProfileRecordVariant where
  compare = gCompare

instance showProfileRecordVariant :: Show ProfileRecordVariant where
  show = gShow

instance enumProfileRecordVariant :: Enum ProfileRecordVariant where
  succ x = case x of
    ProfileVariantChef -> Just ProfileVariantCustomer
    ProfileVariantCustomer -> Just ProfileVariantEditor
    ProfileVariantEditor -> Just ProfileVariantFarmer
    ProfileVariantFarmer -> Just ProfileVariantRestaurant
    ProfileVariantRestaurant -> Nothing
  pred x = case x of
    ProfileVariantChef -> Nothing
    ProfileVariantCustomer -> Just ProfileVariantChef
    ProfileVariantEditor -> Just ProfileVariantCustomer
    ProfileVariantFarmer -> Just ProfileVariantEditor
    ProfileVariantRestaurant -> Just ProfileVariantFarmer

instance boundedProfileRecordVariant :: Bounded ProfileRecordVariant where
  top = ProfileVariantRestaurant
  bottom = ProfileVariantChef

instance boundedEnumProfileRecordVariant :: BoundedEnum ProfileRecordVariant where
  cardinality = Cardinality 5
  toEnum n
    | n == 0 = Just ProfileVariantChef
    | n == 1 = Just ProfileVariantCustomer
    | n == 2 = Just ProfileVariantEditor
    | n == 3 = Just ProfileVariantFarmer
    | n == 4 = Just ProfileVariantRestaurant
    | otherwise = Nothing
  fromEnum x = case x of
    ProfileVariantChef -> 0
    ProfileVariantCustomer -> 1
    ProfileVariantEditor -> 2
    ProfileVariantFarmer -> 3
    ProfileVariantRestaurant -> 4

instance arbitraryProfileRecordVariant :: Arbitrary ProfileRecordVariant where
  arbitrary = oneOf $ NonEmpty
    (pure ProfileVariantChef)
    [ pure ProfileVariantCustomer
    , pure ProfileVariantEditor
    , pure ProfileVariantFarmer
    , pure ProfileVariantRestaurant
    ]

instance encodeJsonProfileRecordVariant :: EncodeJson ProfileRecordVariant where
  encodeJson x = encodeJson $ case x of
    ProfileVariantChef -> "chefProfile"
    ProfileVariantCustomer -> "customerProfile"
    ProfileVariantEditor -> "editorProfile"
    ProfileVariantFarmer -> "farmerProfile"
    ProfileVariantRestaurant -> "restaurantProfile"

instance decodeJsonProfileRecordVariant :: DecodeJson ProfileRecordVariant where
  decodeJson json = do
    s <- decodeJson json
    case unit of
      _ | s == "chefProfile" -> pure ProfileVariantChef
        | s == "customerProfile" -> pure ProfileVariantCustomer
        | s == "editorProfile" -> pure ProfileVariantEditor
        | s == "farmerProfile" -> pure ProfileVariantFarmer
        | s == "restaurantProfile" -> pure ProfileVariantRestaurant
        | otherwise -> fail "ProfileRecordVariant"



data ContentRecordVariant
  = TagRecordVariant TagRecordVariant
  | ChefRecordVariant ChefRecordVariant
  | ProfileRecordVariant ProfileRecordVariant

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
      Nothing -> Just (ProfileRecordVariant bottom)
    ProfileRecordVariant y -> case succ y of
      Just z -> Just (ProfileRecordVariant z)
      Nothing -> Nothing
  pred x = case x of
    TagRecordVariant y -> case pred y of
      Nothing -> Nothing
      Just z -> Just (TagRecordVariant z)
    ChefRecordVariant y -> case pred y of
      Nothing -> Just (TagRecordVariant top)
      Just z -> Just (ChefRecordVariant z)
    ProfileRecordVariant y -> case pred y of
      Nothing -> Just (ChefRecordVariant top)
      Just z -> Just (ProfileRecordVariant z)

instance boundedContentRecordVariant :: Bounded ContentRecordVariant where
  top = ProfileRecordVariant top
  bottom = TagRecordVariant bottom

instance boundedEnumContentRecordVariant :: BoundedEnum ContentRecordVariant where
  cardinality = Cardinality $
      unCardinality (cardinality :: Cardinality TagRecordVariant)
    + unCardinality (cardinality :: Cardinality ChefRecordVariant)
    + unCardinality (cardinality :: Cardinality ProfileRecordVariant)
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
      | n >=  ( unCardinality (cardinality :: Cardinality TagRecordVariant)
              + unCardinality (cardinality :: Cardinality ChefRecordVariant)
              )
        && n < ( unCardinality (cardinality :: Cardinality TagRecordVariant)
               + unCardinality (cardinality :: Cardinality ChefRecordVariant)
               + unCardinality (cardinality :: Cardinality ProfileRecordVariant)
               ) -> ProfileRecordVariant <$> toEnum
                     ( n - ( unCardinality (cardinality :: Cardinality TagRecordVariant)
                           + unCardinality (cardinality :: Cardinality ChefRecordVariant)
                           )
                     )
      | otherwise -> Nothing
  fromEnum x = case x of
    TagRecordVariant y -> fromEnum y
    ChefRecordVariant y -> fromEnum y
                         + unCardinality (cardinality :: Cardinality TagRecordVariant)
    ProfileRecordVariant y -> fromEnum y
                         + unCardinality (cardinality :: Cardinality TagRecordVariant)
                         + unCardinality (cardinality :: Cardinality ChefRecordVariant)

unCardinality :: forall a. Cardinality a -> Int
unCardinality (Cardinality x) = x


instance arbitraryContentRecordVariant :: Arbitrary ContentRecordVariant where
  arbitrary = oneOf $ NonEmpty
    (TagRecordVariant <$> arbitrary)
    [ ChefRecordVariant <$> arbitrary
    , ProfileRecordVariant <$> arbitrary
    ]

instance encodeJsonContentRecordVariant :: EncodeJson ContentRecordVariant where
  encodeJson x = case x of
    TagRecordVariant y -> "tagVariant" := y ~> jsonEmptyObject
    ChefRecordVariant y -> "chefVariant" := y ~> jsonEmptyObject
    ProfileRecordVariant y -> "profileVariant" := y ~> jsonEmptyObject

instance decodeJsonContentRecordVariant :: DecodeJson ContentRecordVariant where
  decodeJson json = do
    o <- decodeJson json
    let tag = TagRecordVariant <$> o .? "tagVariant"
        chef = ChefRecordVariant <$> o .? "chefVariant"
        profile = ProfileRecordVariant <$> o .? "profileVariant"
    tag <|> chef <|> profile

