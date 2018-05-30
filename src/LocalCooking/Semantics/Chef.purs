module LocalCooking.Semantics.Chef where

import LocalCooking.Database.Schema (StoredMealId, StoredOrderId)
import LocalCooking.Common.User.Name (Name)
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Common.Tag.Meal (MealTag)
import LocalCooking.Common.Ingredient (IngredientName)
import LocalCooking.Common.Order (OrderProgress)

import Prelude
import Data.Price (Price)
import Data.String.Markdown (MarkdownText)
import Data.String.Permalink (Permalink)
import Data.Image.Source (ImageSource)
import Data.Date (Date)
import Data.Date.JSON (JSONDate (..), getJSONDate)
import Data.DateTime (DateTime)
import Data.DateTime.JSON (JSONDateTime (..), getJSONDateTime)
import Data.Maybe (Maybe)
import Data.Generic (class Generic, gEq, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, fail, (:=), (~>), jsonEmptyObject, (.?))
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Now (nowDateTime)
import Text.Email.Validate (EmailAddress)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)


newtype ChefSettings = ChefSettings
  { name      :: Name
  , permalink :: Permalink
  , images    :: Array ImageSource
  , avatar    :: ImageSource
  , bio       :: MarkdownText
  , tags      :: Array ChefTag
  }

derive instance genericChefSettings :: Generic ChefSettings

instance eqChefSettings :: Eq ChefSettings where
  eq = gEq

instance showChefSettings :: Show ChefSettings where
  show = gShow

instance arbitraryChefSettings :: Arbitrary ChefSettings where
  arbitrary = do
    name <- arbitrary
    permalink <- arbitrary
    images <- arbitrary
    avatar <- arbitrary
    bio <- arbitrary
    tags <- arbitrary
    pure (ChefSettings {name,permalink,images,avatar,bio,tags})


newtype MenuSettings = MenuSettings
  { published   :: Maybe Date
  , deadline    :: Date
  , heading     :: String
  , description :: MarkdownText
  , tags        :: Array MealTag
  , images      :: Array ImageSource
  }

derive instance genericMenuSettings :: Generic MenuSettings

instance eqMenuSettings :: Eq MenuSettings where
  eq = gEq

instance showMenuSettings :: Show MenuSettings where
  show = gShow

instance arbitraryMenuSettings :: Arbitrary MenuSettings where
  arbitrary = do
    published <- map getJSONDate <$> arbitrary
    JSONDate deadline <- arbitrary
    heading <- arbitrary
    description <- arbitrary
    tags <- arbitrary
    images <- arbitrary
    pure (MenuSettings {published,deadline,heading,description,images,tags})


newtype MealSettings = MealSettings
  { title        :: String
  , permalink    :: Permalink
  , heading      :: String
  , description  :: MarkdownText
  , instructions :: MarkdownText
  , images       :: Array ImageSource
  , ingredients  :: Array IngredientName
  , tags         :: Array MealTag
  , price        :: Price
  }

derive instance genericMealSettings :: Generic MealSettings

instance eqMealSettings :: Eq MealSettings where
  eq = gEq

instance showMealSettings :: Show MealSettings where
  show = gShow

instance arbitraryMealSettings :: Arbitrary MealSettings where
  arbitrary = do
    title <- arbitrary
    permalink <- arbitrary
    heading <- arbitrary
    description <- arbitrary
    instructions <- arbitrary
    images <- arbitrary
    ingredients <- arbitrary
    tags <- arbitrary
    price <- arbitrary
    pure (MealSettings {title,permalink,heading,description,instructions,images,ingredients,tags,price})


newtype Order = Order
  { meal     :: StoredMealId
  , progress :: OrderProgress
  , volume   :: Int
  , id       :: StoredOrderId
  , time     :: DateTime
  }

derive instance genericOrder :: Generic Order

instance eqOrder :: Eq Order where
  eq = gEq

instance showOrder :: Show Order where
  show = gShow

instance arbitraryOrder :: Arbitrary Order where
  arbitrary = do
    meal <- arbitrary
    progress <- arbitrary
    volume <- arbitrary
    id <- arbitrary
    JSONDateTime time <- arbitrary
    pure (Order {meal,progress,volume,id,time})
