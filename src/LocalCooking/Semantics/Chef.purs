module LocalCooking.Semantics.Chef where

import LocalCooking.Database.Schema (StoredMealId, StoredOrderId)
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Common.Tag.Meal (MealTag)
import LocalCooking.Common.Tag.Ingredient (IngredientTag)
import LocalCooking.Common.Order (OrderProgress)

import Prelude
import Data.Name (Name)
import Data.Price (Price)
import Data.String.Markdown (MarkdownText)
import Data.String.Permalink (Permalink)
import Data.Image.Source (ImageSource)
import Data.Date (Date)
import Data.DateTime (DateTime)
import Data.Maybe (Maybe)
import Data.Generic (class Generic, gEq, gShow)
import Data.NonEmpty (NonEmpty (..))
import Data.Argonaut
  ( class EncodeJson, class DecodeJson, encodeJson, decodeJson
  , fail, (:=), (~>), jsonEmptyObject, (.?))
import Data.Argonaut.JSONDate (JSONDate (..), getJSONDate)
import Data.Argonaut.JSONDateTime (JSONDateTime (..))
import Control.Alternative ((<|>))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)


newtype SetChef = SetChef
  { name      :: Maybe Name
  , permalink :: Maybe Permalink
  , images    :: Array ImageSource
  , avatar    :: Maybe ImageSource
  , bio       :: MarkdownText
  , tags      :: Array ChefTag
  }

derive instance genericSetChef :: Generic SetChef

instance eqSetChef :: Eq SetChef where
  eq = gEq

instance showSetChef :: Show SetChef where
  show = gShow

instance arbitrarySetChef :: Arbitrary SetChef where
  arbitrary = do
    name <- arbitrary
    permalink <- arbitrary
    images <- arbitrary
    avatar <- arbitrary
    bio <- arbitrary
    tags <- arbitrary
    pure (SetChef {name,permalink,images,avatar,bio,tags})

instance encodeJsonSetChef :: EncodeJson SetChef where
  encodeJson (SetChef {name,permalink,images,avatar,bio,tags})
    =  "name" := name
    ~> "permalink" := permalink
    ~> "images" := images
    ~> "avatar" := avatar
    ~> "bio" := bio
    ~> "tags" := tags
    ~> jsonEmptyObject

instance decodeJsonSetChef :: DecodeJson SetChef where
  decodeJson json = do
    o <- decodeJson json
    name <- o .? "name"
    permalink <- o .? "permalink"
    images <- o .? "images"
    avatar <- o .? "avatar"
    bio <- o .? "bio"
    tags <- o .? "tags"
    pure (SetChef {name,permalink,images,avatar,bio,tags})

newtype ChefValid = ChefValid
  { name      :: Name
  , permalink :: Permalink
  , images    :: Array ImageSource
  , avatar    :: ImageSource
  , bio       :: MarkdownText
  , tags      :: Array ChefTag
  }

derive instance genericChefValid :: Generic ChefValid

instance eqChefValid :: Eq ChefValid where
  eq = gEq

instance showChefValid :: Show ChefValid where
  show = gShow

instance arbitraryChefValid :: Arbitrary ChefValid where
  arbitrary = do
    name <- arbitrary
    permalink <- arbitrary
    images <- arbitrary
    avatar <- arbitrary
    bio <- arbitrary
    tags <- arbitrary
    pure (ChefValid {name,permalink,images,avatar,bio,tags})

instance encodeJsonChefValid :: EncodeJson ChefValid where
  encodeJson (ChefValid {name,permalink,images,avatar,bio,tags})
    =  "name" := name
    ~> "permalink" := permalink
    ~> "images" := images
    ~> "avatar" := avatar
    ~> "bio" := bio
    ~> "tags" := tags
    ~> jsonEmptyObject

instance decodeJsonChefValid :: DecodeJson ChefValid where
  decodeJson json = do
    o <- decodeJson json
    name <- o .? "name"
    permalink <- o .? "permalink"
    images <- o .? "images"
    avatar <- o .? "avatar"
    bio <- o .? "bio"
    tags <- o .? "tags"
    pure (ChefValid {name,permalink,images,avatar,bio,tags})


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
  , ingredients  :: Array IngredientTag
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


-- * Errors



data ChefExists a
  = ChefDoesntExist
  | ChefExists a

derive instance genericChefExists :: Generic a => Generic (ChefExists a)

instance arbitraryChefExists :: Arbitrary a => Arbitrary (ChefExists a) where
  arbitrary = oneOf $ NonEmpty
    ( pure ChefDoesntExist
    )
    [ ChefExists <$> arbitrary
    ]

instance eqChefExists :: Generic a => Eq (ChefExists a) where
  eq = gEq

instance showChefExists :: Generic a => Show (ChefExists a) where
  show = gShow

instance encodeJsonChefExists :: EncodeJson a => EncodeJson (ChefExists a) where
  encodeJson x = case x of
    ChefDoesntExist -> encodeJson "chefDoesntExist"
    ChefExists y
      -> "chefExists"
      := y
      ~> jsonEmptyObject

instance decodeJsonChefExists :: DecodeJson a => DecodeJson (ChefExists a) where
  decodeJson json = do
    let empty = do
          s <- decodeJson json
          if s == "chefDoesntExist"
             then pure ChefDoesntExist
             else fail "Not a ChefExists"
        has = do
          o <- decodeJson json
          ChefExists <$> o .? "chefExists"
    empty <|> has


data ChefUnique a
  = ChefNotUnique
  | ChefUnique a

derive instance genericChefUnique :: Generic a => Generic (ChefUnique a)

instance arbitraryChefUnique :: Arbitrary a => Arbitrary (ChefUnique a) where
  arbitrary = oneOf $ NonEmpty
    ( pure ChefNotUnique
    )
    [ ChefUnique <$> arbitrary
    ]

instance eqChefUnique :: Generic a => Eq (ChefUnique a) where
  eq = gEq

instance showChefUnique :: Generic a => Show (ChefUnique a) where
  show = gShow

instance encodeJsonChefUnique :: EncodeJson a => EncodeJson (ChefUnique a) where
  encodeJson x = case x of
    ChefNotUnique -> encodeJson "chefNotUnique"
    ChefUnique y
      -> "chefUnique"
      := y
      ~> jsonEmptyObject

instance decodeJsonChefUnique :: DecodeJson a => DecodeJson (ChefUnique a) where
  decodeJson json = do
    let empty = do
          s <- decodeJson json
          if s == "chefNotUnique"
             then pure ChefNotUnique
             else fail "Not a ChefUnique"
        has = do
          o <- decodeJson json
          ChefUnique <$> o .? "chefUnique"
    empty <|> has
