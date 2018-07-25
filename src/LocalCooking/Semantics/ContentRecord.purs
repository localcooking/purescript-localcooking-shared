module LocalCooking.Semantics.ContentRecord where

import LocalCooking.Semantics.Chef (SetChef, MenuSettings, MealSettings)
import LocalCooking.Semantics.Mitch (SetCustomer)
import LocalCooking.Semantics.Content (SetEditor)
import LocalCooking.Database.Schema (StoredMenuId, StoredMealId)
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Common.Tag.Culture (CultureTag)
import LocalCooking.Common.Tag.Diet (DietTag)
import LocalCooking.Common.Tag.Farm (FarmTag)
import LocalCooking.Common.Tag.Ingredient (IngredientTag)
import LocalCooking.Common.Tag.Meal (MealTag)

import Prelude
import Data.NonEmpty (NonEmpty (..))
import Data.Generic (class Generic, gEq, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, (~>), (:=), jsonEmptyObject, (.?))
import Data.Argonaut.JSONTuple (JSONTuple)
import Control.Alternative ((<|>))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)


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
  = ChefRecordSetMenu (JSONTuple StoredMenuId MenuSettings)
  | ChefRecordNewMenu MenuSettings
  | ChefRecordSetMeal (JSONTuple StoredMenuId (JSONTuple StoredMealId MealSettings))
  | ChefRecordNewMeal (JSONTuple StoredMenuId MealSettings)

derive instance genericChefRecord :: Generic ChefRecord

instance eqChefRecord :: Eq ChefRecord where
  eq = gEq

instance showChefRecord :: Show ChefRecord where
  show = gShow

instance arbitraryChefRecord :: Arbitrary ChefRecord where
  arbitrary = oneOf $ NonEmpty
    (ChefRecordSetMenu <$> arbitrary)
    [ ChefRecordNewMenu <$> arbitrary
    , ChefRecordSetMeal <$> arbitrary
    , ChefRecordNewMeal <$> arbitrary
    ]

instance encodeJsonChefRecord :: EncodeJson ChefRecord where
  encodeJson x = case x of
    ChefRecordSetMenu y -> "setMenu" := y ~> jsonEmptyObject
    ChefRecordNewMenu y -> "newMenu" := y ~> jsonEmptyObject
    ChefRecordSetMeal y -> "setMeal" := y ~> jsonEmptyObject
    ChefRecordNewMeal y -> "newMeal" := y ~> jsonEmptyObject

instance decodeJsonChefRecord :: DecodeJson ChefRecord where
  decodeJson json = do
    o <- decodeJson json
    let setMenu = ChefRecordSetMenu <$> o .? "setMenu"
        newMenu = ChefRecordNewMenu <$> o .? "newMenu"
        setMeal = ChefRecordSetMeal <$> o .? "setMeal"
        newMeal = ChefRecordNewMeal <$> o .? "newMeal"
    setMenu <|> newMenu <|> setMeal <|> newMeal

data ProfileRecord
  = ProfileRecordChef SetChef
  | ProfileRecordCustomer SetCustomer
  | ProfileRecordEditor SetEditor

derive instance genericProfileRecord :: Generic ProfileRecord

instance eqProfileRecord :: Eq ProfileRecord where
  eq = gEq

instance showProfileRecord :: Show ProfileRecord where
  show = gShow

instance arbitraryProfileRecord :: Arbitrary ProfileRecord where
  arbitrary = oneOf $ NonEmpty
    (ProfileRecordChef <$> arbitrary)
    [ ProfileRecordCustomer <$> arbitrary
    , ProfileRecordEditor <$> arbitrary
    ]

instance encodeJsonProfileRecord :: EncodeJson ProfileRecord where
  encodeJson x = case x of
    ProfileRecordChef y -> "chef" := y ~> jsonEmptyObject
    ProfileRecordCustomer y -> "customer" := y ~> jsonEmptyObject
    ProfileRecordEditor y -> "editor" := y ~> jsonEmptyObject

instance decodeJsonProfileRecord :: DecodeJson ProfileRecord where
  decodeJson json = do
    o <- decodeJson json
    let chef = ProfileRecordChef <$> o .? "chef"
        customer = ProfileRecordCustomer <$> o .? "customer"
        editor = ProfileRecordEditor <$> o .? "editor"
    chef <|> customer <|> editor


data ContentRecord
  = TagRecord TagRecord
  | ChefRecord ChefRecord
  | ProfileRecord ProfileRecord

derive instance genericContentRecord :: Generic ContentRecord

instance eqContentRecord :: Eq ContentRecord where
  eq = gEq

instance showContentRecord :: Show ContentRecord where
  show = gShow

instance arbitraryContentRecord :: Arbitrary ContentRecord where
  arbitrary = oneOf $ NonEmpty
    (TagRecord <$> arbitrary)
    [ ChefRecord <$> arbitrary
    , ProfileRecord <$> arbitrary
    ]

instance encodeJsonContentRecord :: EncodeJson ContentRecord where
  encodeJson x = case x of
    TagRecord y -> "tagRecord" := y ~> jsonEmptyObject
    ChefRecord y -> "chefRecord" := y ~> jsonEmptyObject
    ProfileRecord y -> "profileRecord" := y ~> jsonEmptyObject

instance decodeJsonContentRecord :: DecodeJson ContentRecord where
  decodeJson json = do
    o <- decodeJson json
    let tag = TagRecord <$> o .? "tagRecord"
        chef = ChefRecord <$> o .? "chefRecord"
        profile = ProfileRecord <$> o .? "profileRecord"
    tag <|> chef <|> profile
