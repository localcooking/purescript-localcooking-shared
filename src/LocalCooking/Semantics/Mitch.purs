module LocalCooking.Semantics.Mitch where

import LocalCooking.Database.Schema (StoredMealId, StoredReviewId)
import LocalCooking.Common.User.Name (Name)
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Common.Tag.Meal (MealTag)
import LocalCooking.Common.Tag.Ingredient (IngredientTag)
import LocalCooking.Common.Tag.Diet (DietTag)
import LocalCooking.Common.Ingredient (Ingredient)
import LocalCooking.Common.Order (OrderProgress)
import LocalCooking.Common.Rating (Rating)

import Prelude
import Data.Address (USAAddress)
import Data.Price (Price)
import Data.String.Markdown (MarkdownText)
import Data.String.Permalink (Permalink)
import Data.Image.Source (ImageSource)
import Data.Date (Date)
import Data.Date.JSON (JSONDate (..), getJSONDate)
import Data.DateTime (DateTime)
import Data.DateTime.JSON (JSONDateTime (..))
import Data.Generic (class Generic, gEq, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, (:=), (~>), jsonEmptyObject, (.?))
import Test.QuickCheck (class Arbitrary, arbitrary)




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

instance arbitraryReviewSynopsis :: Arbitrary ReviewSynopsis where
  arbitrary = do
    rating <- arbitrary
    heading <- arbitrary
    id <- arbitrary
    pure (ReviewSynopsis {rating,heading,id})

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

instance arbitraryReview :: Arbitrary Review where
  arbitrary = do
    rating <- arbitrary
    JSONDateTime submitted <- arbitrary
    heading <- arbitrary
    id <- arbitrary
    body <- arbitrary
    images <- arbitrary
    pure (Review {rating,submitted,heading,id,body,images})

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

instance arbitraryMenuSynopsis :: Arbitrary MenuSynopsis where
  arbitrary = do
    published <- getJSONDate <$> arbitrary
    deadline <- getJSONDate <$> arbitrary
    headline <- arbitrary
    tags <- arbitrary
    images <- arbitrary
    pure (MenuSynopsis {published,deadline,headline,tags,images})

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

instance arbitraryMenu :: Arbitrary Menu where
  arbitrary = do
    published <- getJSONDate <$> arbitrary
    deadline <- getJSONDate <$> arbitrary
    description <- arbitrary
    author <- arbitrary
    meals <- arbitrary
    pure (Menu {published,deadline,description,author,meals})

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
  , diets     :: Array DietTag
  , price     :: Price
  }

derive instance genericMealSynopsis :: Generic MealSynopsis

instance eqMealSynopsis :: Eq MealSynopsis where
  eq = gEq

instance showMealSynopsis :: Show MealSynopsis where
  show = gShow

instance arbitraryMealSynopsis :: Arbitrary MealSynopsis where
  arbitrary = do
    title <- arbitrary
    permalink <- arbitrary
    heading <- arbitrary
    images <- arbitrary
    rating <- arbitrary
    orders <- arbitrary
    tags <- arbitrary
    diets <- arbitrary
    price <- arbitrary
    pure (MealSynopsis {title,permalink,heading,images,rating,orders,tags,diets,price})

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
  , diets        :: Array DietTag
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

instance arbitraryMeal :: Arbitrary Meal where
  arbitrary = do
    title <- arbitrary
    permalink <- arbitrary
    description <- arbitrary
    instructions <- arbitrary
    images <- arbitrary
    ingredients <- arbitrary
    diets <- arbitrary
    tags <- arbitrary
    orders <- arbitrary
    rating <- arbitrary
    reviews <- arbitrary
    price <- arbitrary
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

instance arbitraryChefSynopsis :: Arbitrary ChefSynopsis where
  arbitrary = do
    name <- arbitrary
    permalink <- arbitrary
    image <- arbitrary
    rating <- arbitrary
    orders <- arbitrary
    tags <- arbitrary
    pure (ChefSynopsis {name,permalink,image,rating,orders,tags})

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



newtype Chef = Chef
  { name         :: Name
  , permalink    :: Permalink
  , images       :: Array ImageSource
  , bio          :: MarkdownText
  , rating       :: Rating
  , reviews      :: Array ReviewSynopsis
  , activeOrders :: Int
  , totalOrders  :: Int
  , tags         :: Array ChefTag
  , menus        :: Array MenuSynopsis
  }

derive instance genericChef :: Generic Chef

instance eqChef :: Eq Chef where
  eq = gEq

instance showChef :: Show Chef where
  show = gShow

instance arbitraryChef :: Arbitrary Chef where
  arbitrary = do
    name <- arbitrary
    permalink <- arbitrary
    images <- arbitrary
    bio <- arbitrary
    rating <- arbitrary
    reviews <- arbitrary
    activeOrders <- arbitrary
    totalOrders <- arbitrary
    tags <- arbitrary
    menus <- arbitrary
    pure (Chef {name,permalink,images,bio,rating,reviews,activeOrders,totalOrders,tags,menus})

instance encodeJsonChef :: EncodeJson Chef where
  encodeJson (Chef {name,permalink,images,bio,rating,reviews,activeOrders,totalOrders,tags,menus})
    =  "name" := name
    ~> "permalink" := permalink
    ~> "images" := images
    ~> "bio" := bio
    ~> "rating" := rating
    ~> "reviews" := reviews
    ~> "activeOrders" := activeOrders
    ~> "totalOrders" := totalOrders
    ~> "tags" := tags
    ~> "menus" := menus
    ~> jsonEmptyObject

instance decodeJsonChef :: DecodeJson Chef where
  decodeJson json = do
    o <- decodeJson json
    name <- o .? "name"
    permalink <- o .? "permalink"
    images <- o .? "images"
    bio <- o .? "bio"
    rating <- o .? "rating"
    reviews <- o .? "reviews"
    activeOrders <- o .? "activeOrders"
    totalOrders <- o .? "totalOrders"
    tags <- o .? "tags"
    menus <- o .? "menus"
    pure (Chef {name,permalink,images,bio,rating,reviews,activeOrders,totalOrders,tags,menus})



newtype Order = Order
  { meal     :: MealSynopsis
  , progress :: OrderProgress
  , time     :: DateTime
  , volume   :: Int
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
    JSONDateTime time <- arbitrary
    volume <- arbitrary
    pure (Order {meal,progress,time,volume})

instance encodeJsonOrder :: EncodeJson Order where
  encodeJson (Order {meal,progress,time,volume})
    =  "meal" := meal
    ~> "progress" := progress
    ~> "time" := JSONDateTime time
    ~> "volume" := volume
    ~> jsonEmptyObject

instance decodeJsonOrder :: DecodeJson Order where
  decodeJson json = do
    o <- decodeJson json
    meal <- o .? "meal"
    progress <- o .? "progress"
    JSONDateTime time <- o .? "time"
    volume <- o .? "volume"
    pure (Order {meal,progress,time,volume})



newtype GetSetCustomer = GetSetCustomer
  { name    :: Name
  , address :: USAAddress
  }

derive instance genericGetSetCustomer :: Generic GetSetCustomer

instance eqGetSetCustomer :: Eq GetSetCustomer where
  eq = gEq

instance showGetSetCustomer :: Show GetSetCustomer where
  show = gShow

instance arbitraryGetSetCustomer :: Arbitrary GetSetCustomer where
  arbitrary = do
    name <- arbitrary
    address <- arbitrary
    pure (GetSetCustomer {name,address})

instance encodeJsonGetSetCustomer :: EncodeJson GetSetCustomer where
  encodeJson (GetSetCustomer {name,address})
    =  "name" := name
    ~> "address" := address
    ~> jsonEmptyObject

instance decodeJsonGetSetCustomer :: DecodeJson GetSetCustomer where
  decodeJson json = do
    o <- decodeJson json
    name <- o .? "name"
    address <- o .? "address"
    pure (GetSetCustomer {name,address})


newtype Diets = Diets (Array DietTag)
derive instance genericDiets :: Generic Diets
derive newtype instance eqDiets :: Eq Diets
derive newtype instance showDiets :: Show Diets
derive newtype instance arbitraryDiets :: Arbitrary Diets
derive newtype instance encodeJsonDiets :: EncodeJson Diets
derive newtype instance decodeJsonDiets :: DecodeJson Diets

newtype Allergies = Allergies (Array IngredientTag)
derive instance genericAllergies :: Generic Allergies
derive newtype instance eqAllergies :: Eq Allergies
derive newtype instance showAllergies :: Show Allergies
derive newtype instance arbitraryAllergies :: Arbitrary Allergies
derive newtype instance encodeJsonAllergies :: EncodeJson Allergies
derive newtype instance decodeJsonAllergies :: DecodeJson Allergies



newtype CartEntry = CartEntry
  { meal   :: StoredMealId
  , volume :: Int
  , added  :: DateTime
  }

derive instance genericCartEntry :: Generic CartEntry

instance eqCartEntry :: Eq CartEntry where
  eq = gEq

instance showCartEntry :: Show CartEntry where
  show = gShow

instance arbitraryCartEntry :: Arbitrary CartEntry where
  arbitrary = do
    meal <- arbitrary
    volume <- arbitrary
    JSONDateTime added <- arbitrary
    pure (CartEntry {meal,volume,added})

instance encodeJsonCartEntry :: EncodeJson CartEntry where
  encodeJson (CartEntry {meal,volume,added})
    =  "meal" := meal
    ~> "volume" := volume
    ~> "added" := JSONDateTime added
    ~> jsonEmptyObject

instance decodeJsonCartEntry :: DecodeJson CartEntry where
  decodeJson json = do
    o <- decodeJson json
    meal <- o .? "meal"
    volume <- o .? "volume"
    JSONDateTime added <- o .? "added"
    pure (CartEntry {meal,volume,added})
