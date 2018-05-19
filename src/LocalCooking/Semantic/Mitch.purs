
module LocalCooking.Semantic.Mitch where

import LocalCooking.Common.User.Name (Name)
import LocalCooking.Common.Ingredient (Ingredient)
import LocalCooking.Common.Rating (Rating)
import LocalCooking.Common.Diet (Diet)
import LocalCooking.Common.Tag.Meal (MealTag)
import LocalCooking.Common.Tag.Chef (ChefTag)

import Prelude
import Data.Price (Price)
import Data.Date (Date)
import Data.Date.JSON (JSONDate (..), getJSONDate)
import Data.Image.Source (ImageSource)
import Data.String.Markdown (MarkdownText)
import Data.String.Permalink (Permalink)
import Data.Generic (class Generic, gShow, gEq)
import Data.Argonaut (class EncodeJson, class DecodeJson, (~>), (:=), decodeJson, (.?), jsonEmptyObject)
import Test.QuickCheck (class Arbitrary)



-- * Reviews

newtype ReviewId = ReviewId Int

derive instance genericReviewId :: Generic ReviewId
derive newtype instance arbitraryReviewId :: Arbitrary ReviewId
derive newtype instance eqReviewId :: Eq ReviewId
derive newtype instance ordReviewId :: Ord ReviewId
derive newtype instance showReviewId :: Show ReviewId
derive newtype instance encodeJsonReviewId :: EncodeJson ReviewId
derive newtype instance decodeJsonReviewId :: DecodeJson ReviewId


newtype ReviewSynopsis = ReviewSynopsis
  { rating  :: Rating
  , heading :: String
  , id      :: ReviewId
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


newtype Review = Review
  { rating  :: Rating
  , heading :: String
  , id      :: ReviewId
  , body    :: MarkdownText
  , images  :: Array ImageSource
  }


derive instance genericReview :: Generic Review

instance eqReview :: Eq Review where
  eq = gEq

instance showReview :: Show Review where
  show = gShow

instance encodeJsonReview :: EncodeJson Review where
  encodeJson (Review {rating,heading,body,images,id})
    =  "rating" := rating
    ~> "heading" := heading
    ~> "id" := id
    ~> "body" := body
    ~> "images" := images
    ~> jsonEmptyObject

instance decodeJsonReview :: DecodeJson Review where
  decodeJson json = do
    o <- decodeJson json
    rating <- o .? "rating"
    heading <- o .? "heading"
    id <- o .? "id"
    body <- o .? "body"
    images <- o .? "images"
    pure (Review {rating,heading,id,body,images})


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


-- * Chefs

newtype ChefSynopsis = ChefSynopsis
  { name      :: Name
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
  encodeJson (ChefSynopsis {name,image,rating,orders,tags})
    =  "name" := name
    ~> "image" := image
    ~> "rating" := rating
    ~> "orders" := orders
    ~> "tags" := tags
    ~> jsonEmptyObject

instance decodeJsonChefSynopsis :: DecodeJson ChefSynopsis where
  decodeJson json = do
    o <- decodeJson json
    name <- o .? "name"
    image <- o .? "image"
    rating <- o .? "rating"
    orders <- o .? "orders"
    tags <- o .? "tags"
    pure (ChefSynopsis {name,image,rating,orders,tags})
