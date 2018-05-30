module LocalCooking.Semantics.Mitch where

import LocalCooking.Database.Schema (StoredMealId, StoredOrderId, StoredReviewId)
import LocalCooking.Common.User.Name (Name)
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Common.Tag.Meal (MealTag)
import LocalCooking.Common.Ingredient (Ingredient)
import LocalCooking.Common.Order (OrderProgress)
import LocalCooking.Common.Diet (Diet)
import LocalCooking.Common.Rating (Rating)

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




-- TODO FIXME arbitrary instances


newtype ReviewSynopsis = ReviewSynopsis
  { rating  :: Rating
  , heading :: String
  , id      :: StoredReviewId
  }

derive instance genericReviewSynopsis :: Generic ReviewSynopsis

instance eqReviewSynopsis :: Eq ReviewSynopsis where
  eq = gEq

instance showReviewSynopsis :: Show ReviewSynopsis where
  show = gShow

instance encodeJsonReviewSynopsis :: EncodeJson ReviewSynopsis where
  encodeJson (ReviewSynopsis {rating,heading,id})
    =  "rating" := rating
    ~> "heading" := heading
    ~> "id" := id
    ~> jsonEmptyObject

instance decodeJsonReviewSynopsis :: DecodeJson ReviewSynopsis where
  decodeJson json = do
    o <- decodeJson json
    rating <- o .? "rating"
    heading <- o .? "heading"
    id <- o .? "id"
    pure (ReviewSynopsis {rating,heading,id})


getReviewSynopsis :: Review -> ReviewSynopsis
getReviewSynopsis (Review {rating,heading,id}) = ReviewSynopsis
  { rating  : rating
  , id      : id
  , heading : heading
  }


newtype Review = Review
  { rating    :: Rating
  , submitted :: DateTime
  , heading   :: String
  , id        :: StoredReviewId
  , body      :: MarkdownText
  , images    :: Array ImageSource
  }


derive instance genericReview :: Generic Review

instance eqReview :: Eq Review where
  eq = gEq

instance showReview :: Show Review where
  show = gShow

instance encodeJsonReview :: EncodeJson Review where
  encodeJson (Review {rating,submitted,heading,body,images,id})
    =  "rating" := rating
    ~> "submitted" := JSONDateTime submitted
    ~> "heading" := heading
    ~> "id" := id
    ~> "body" := body
    ~> "images" := images
    ~> jsonEmptyObject

instance decodeJsonReview :: DecodeJson Review where
  decodeJson json = do
    o <- decodeJson json
    rating <- o .? "rating"
    JSONDateTime submitted <- o .? "submitted"
    heading <- o .? "heading"
    id <- o .? "id"
    body <- o .? "body"
    images <- o .? "images"
    pure (Review {rating,submitted,heading,id,body,images})


-- * Menus

newtype MenuSynopsis = MenuSynopsis
  { published :: Date
  , deadline  :: Date
  , headline  :: String
  , tags      :: Array MealTag
  , images    :: Array ImageSource
  }

derive instance genericMenuSynopsis :: Generic MenuSynopsis

instance eqMenuSynopsis :: Eq MenuSynopsis where
  eq = gEq

instance showMenuSynopsis :: Show MenuSynopsis where
  show = gShow

instance encodeJsonMenuSynopsis :: EncodeJson MenuSynopsis where
  encodeJson (MenuSynopsis {published,deadline,headline,tags,images})
    =  "published" := JSONDate published
    ~> "deadline" := JSONDate deadline
    ~> "headline" := headline
    ~> "tags" := tags
    ~> "images" := images
    ~> jsonEmptyObject

instance decodeJsonMenuSynopsis :: DecodeJson MenuSynopsis where
  decodeJson json = do
    o <- decodeJson json
    published <- getJSONDate <$> o .? "published"
    deadline <- getJSONDate <$> o .? "deadline"
    headline <- o .? "headline"
    tags <- o .? "tags"
    images <- o .? "images"
    pure (MenuSynopsis {published,deadline,headline,tags,images})


newtype Menu = Menu
  { published   :: Date
  , deadline    :: Date
  , description :: MarkdownText
  , author      :: ChefSynopsis
  , meals       :: Array MealSynopsis
  }

derive instance genericMenu :: Generic Menu

instance eqMenu :: Eq Menu where
  eq = gEq

instance showMenu :: Show Menu where
  show = gShow

instance encodeJsonMenu :: EncodeJson Menu where
  encodeJson (Menu {published,deadline,description,author,meals})
    =  "published" := JSONDate published
    ~> "deadline" := JSONDate deadline
    ~> "description" := description
    ~> "author" := author
    ~> "meals" := meals
    ~> jsonEmptyObject

instance decodeJsonMenu :: DecodeJson Menu where
  decodeJson json = do
    o <- decodeJson json
    published <- getJSONDate <$> o .? "published"
    deadline <- getJSONDate <$> o .? "deadline"
    description <- o .? "description"
    author <- o .? "author"
    meals <- o .? "meals"
    pure (Menu {published,deadline,description,author,meals})


-- * Meals

newtype MealSynopsis = MealSynopsis
  { title     :: String
  , permalink :: Permalink
  , heading   :: String
  , images    :: Array ImageSource
  , rating    :: Rating
  , orders    :: Int
  , tags      :: Array MealTag
  , diets     :: Array Diet
  , price     :: Price
  }

derive instance genericMealSynopsis :: Generic MealSynopsis

instance eqMealSynopsis :: Eq MealSynopsis where
  eq = gEq

instance showMealSynopsis :: Show MealSynopsis where
  show = gShow

instance encodeJsonMealSynopsis :: EncodeJson MealSynopsis where
  encodeJson (MealSynopsis {title,permalink,heading,images,rating,orders,tags,diets,price})
    =  "title" := title
    ~> "permalink" := permalink
    ~> "heading" := heading
    ~> "images" := images
    ~> "rating" := rating
    ~> "orders" := orders
    ~> "tags" := tags
    ~> "diets" := diets
    ~> "price" := price
    ~> jsonEmptyObject

instance decodeJsonMealSynopsis :: DecodeJson MealSynopsis where
  decodeJson json = do
    o <- decodeJson json
    title <- o .? "title"
    permalink <- o .? "permalink"
    heading <- o .? "heading"
    images <- o .? "images"
    rating <- o .? "rating"
    orders <- o .? "orders"
    tags <- o .? "tags"
    diets <- o .? "diets"
    price <- o .? "price"
    pure (MealSynopsis {title,permalink,heading,images,rating,orders,tags,diets,price})


newtype Meal = Meal
  { title        :: String
  , permalink    :: Permalink
  , description  :: MarkdownText
  , instructions :: MarkdownText
  , images       :: Array ImageSource
  , ingredients  :: Array Ingredient
  , diets        :: Array Diet
  , tags         :: Array MealTag
  , orders       :: Int
  , rating       :: Rating
  , reviews      :: Array ReviewSynopsis
  , price        :: Price
  }


derive instance genericMeal :: Generic Meal

instance eqMeal :: Eq Meal where
  eq = gEq

instance showMeal :: Show Meal where
  show = gShow

instance encodeJsonMeal :: EncodeJson Meal where
  encodeJson
    ( Meal
      { title
      , permalink
      , description
      , instructions
      , images
      , ingredients
      , rating
      , orders
      , tags
      , diets
      , reviews
      , price
      }
    )
    =  "title" := title
    ~> "permalink" := permalink
    ~> "description" := description
    ~> "instructions" := instructions
    ~> "images" := images
    ~> "ingredients" := ingredients
    ~> "diets" := diets
    ~> "tags" := tags
    ~> "orders" := orders
    ~> "rating" := rating
    ~> "reviews" := reviews
    ~> "price" := price
    ~> jsonEmptyObject

instance decodeJsonMeal :: DecodeJson Meal where
  decodeJson json = do
    o <- decodeJson json
    title <- o .? "title"
    permalink <- o .? "permalink"
    description <- o .? "description"
    instructions <- o .? "instructions"
    images <- o .? "images"
    ingredients <- o .? "ingredients"
    diets <- o .? "diets"
    tags <- o .? "tags"
    orders <- o .? "orders"
    rating <- o .? "rating"
    reviews <- o .? "reviews"
    price <- o .? "price"
    pure $ Meal
      { title
      , permalink
      , description
      , instructions
      , images
      , ingredients
      , rating
      , orders
      , tags
      , diets
      , reviews
      , price
      }


-- * Chefs

newtype ChefSynopsis = ChefSynopsis
  { name      :: Name
  , permalink :: Permalink
  , image     :: ImageSource
  , rating    :: Rating
  , orders    :: Int
  , tags      :: Array ChefTag
  }

derive instance genericChefSynopsis :: Generic ChefSynopsis

instance eqChefSynopsis :: Eq ChefSynopsis where
  eq = gEq

instance showChefSynopsis :: Show ChefSynopsis where
  show = gShow

instance encodeJsonChefSynopsis :: EncodeJson ChefSynopsis where
  encodeJson (ChefSynopsis {name,permalink,image,rating,orders,tags})
    =  "name" := name
    ~> "permalink" := permalink
    ~> "image" := image
    ~> "rating" := rating
    ~> "orders" := orders
    ~> "tags" := tags
    ~> jsonEmptyObject

instance decodeJsonChefSynopsis :: DecodeJson ChefSynopsis where
  decodeJson json = do
    o <- decodeJson json
    name <- o .? "name"
    permalink <- o .? "permalink"
    image <- o .? "image"
    rating <- o .? "rating"
    orders <- o .? "orders"
    tags <- o .? "tags"
    pure (ChefSynopsis {name,permalink,image,rating,orders,tags})
