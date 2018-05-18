module LocalCooking.Semantic.Mitch.Meal where

import LocalCooking.Semantic.Mitch.Review (ReviewSynopsis)
import LocalCooking.Common.Rating (Rating)
import LocalCooking.Common.Tag.Meal (MealTag)
import LocalCooking.Common.Diet (Diet)
import LocalCooking.Common.Ingredient (Ingredient)

import Prelude
import Data.String.Markdown (MarkdownText)
import Data.String.Permalink (Permalink)
import Data.Image.Source (ImageSource)
import Data.Generic (class Generic, gShow, gEq)
import Data.Argonaut (class EncodeJson, class DecodeJson, (~>), (:=), decodeJson, (.?), jsonEmptyObject)


newtype MealSynopsis = MealSynopsis
  { title     :: String
  , permalink :: Permalink
  , heading   :: String
  , images    :: Array ImageSource
  , rating    :: Rating
  , orders    :: Int
  , tags      :: Array MealTag
  , diets     :: Array Diet
  }

derive instance genericMealSynopsis :: Generic MealSynopsis

instance eqMealSynopsis :: Eq MealSynopsis where
  eq = gEq

instance showMealSynopsis :: Show MealSynopsis where
  show = gShow

instance encodeJsonMealSynopsis :: EncodeJson MealSynopsis where
  encodeJson (MealSynopsis {title,permalink,heading,images,rating,orders,tags,diets})
    =  "title" := title
    ~> "permalink" := permalink
    ~> "heading" := heading
    ~> "images" := images
    ~> "rating" := rating
    ~> "orders" := orders
    ~> "tags" := tags
    ~> "diets" := diets
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
    pure (MealSynopsis {title,permalink,heading,images,rating,orders,tags,diets})


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
      }
