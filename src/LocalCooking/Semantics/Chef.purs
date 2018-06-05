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
import Data.DateTime.JSON (JSONDateTime (..))
import Data.Maybe (Maybe)
import Data.Generic (class Generic, gEq, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, (:=), (~>), jsonEmptyObject, (.?))
import Test.QuickCheck (class Arbitrary, arbitrary)


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

instance encodeJsonChefSettings :: EncodeJson ChefSettings where
  encodeJson (ChefSettings {name,permalink,images,avatar,bio,tags})
    =  "name" := name
    ~> "permalink" := permalink
    ~> "images" := images
    ~> "avatar" := avatar
    ~> "bio" := bio
    ~> "tags" := tags
    ~> jsonEmptyObject

instance decodeJsonChefSettings :: DecodeJson ChefSettings where
  decodeJson json = do
    o <- decodeJson json
    name <- o .? "name"
    permalink <- o .? "permalink"
    images <- o .? "images"
    avatar <- o .? "avatar"
    bio <- o .? "bio"
    tags <- o .? "tags"
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

instance encodeJsonMenuSettings :: EncodeJson MenuSettings where
  encodeJson (MenuSettings {published,deadline,heading,description,tags,images})
    =  "published" := map JSONDate published
    ~> "deadline" := JSONDate deadline
    ~> "heading" := heading
    ~> "description" := description
    ~> "tags" := tags
    ~> "images" := images
    ~> jsonEmptyObject

instance decodeJsonMenuSettings :: DecodeJson MenuSettings where
  decodeJson json = do
    o <- decodeJson json
    published <- map getJSONDate <$> o .? "published"
    JSONDate deadline <- o .? "deadline"
    heading <- o .? "heading"
    description <- o .? "description"
    tags <- o .? "tags"
    images <- o .? "images"
    pure (MenuSettings {published,deadline,heading,description,tags,images})


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

instance encodeJsonMealSettings :: EncodeJson MealSettings where
  encodeJson (MealSettings {title,permalink,heading,description,instructions,images,ingredients,tags,price})
    =  "title" := title
    ~> "permalink" := permalink
    ~> "heading" := heading
    ~> "description" := description
    ~> "instructions" := instructions
    ~> "images" := images
    ~> "ingredients" := ingredients
    ~> "tags" := tags
    ~> "price" := price
    ~> jsonEmptyObject

instance decodeJsonMealSettings :: DecodeJson MealSettings where
  decodeJson json = do
    o <- decodeJson json
    title <- o .? "title"
    permalink <- o .? "permalink"
    heading <- o .? "heading"
    description <- o .? "description"
    instructions <- o .? "instructions"
    images <- o .? "images"
    ingredients <- o .? "ingredients"
    tags <- o .? "tags"
    price <- o .? "price"
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

instance encodeJsonOrder :: EncodeJson Order where
  encodeJson (Order {meal,progress,volume,id,time})
    =  "meal" := meal
    ~> "progress" := progress
    ~> "volume" := volume
    ~> "id" := id
    ~> "time" := JSONDateTime time
    ~> jsonEmptyObject

instance decodeJsonOrder :: DecodeJson Order where
  decodeJson json = do
    o <- decodeJson json
    meal <- o .? "meal"
    progress <- o .? "progress"
    volume <- o .? "volume"
    id <- o .? "id"
    JSONDateTime time <- o .? "time"
    pure (Order {meal,progress,volume,id,time})
