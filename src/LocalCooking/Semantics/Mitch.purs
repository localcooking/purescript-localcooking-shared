module LocalCooking.Semantics.Mitch where

import LocalCooking.Database.Schema (StoredMealId, StoredReviewId, StoredOrderId)
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Common.Tag.Meal (MealTag)
import LocalCooking.Common.Tag.Ingredient (IngredientTag)
import LocalCooking.Common.Tag.Diet (DietTag)
import LocalCooking.Common.Ingredient (Ingredient)
import LocalCooking.Common.Order (OrderProgress)
import LocalCooking.Common.Rating (Rating)

import Prelude
import Data.Name (Name)
import Data.Address (USAAddress)
import Data.Price (Price)
import Data.String.Markdown (MarkdownText)
import Data.String.Permalink (Permalink)
import Data.Image.Source (ImageSource)
import Data.Date (Date)
import Data.DateTime (DateTime)
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




newtype SetCustomer = SetCustomer
  { name    :: Name
  , address :: USAAddress
  }

derive instance genericSetCustomer :: Generic SetCustomer

instance eqSetCustomer :: Eq SetCustomer where
  eq = gEq

instance showSetCustomer :: Show SetCustomer where
  show = gShow

instance arbitrarySetCustomer :: Arbitrary SetCustomer where
  arbitrary = do
    name <- arbitrary
    address <- arbitrary
    pure (SetCustomer {name,address})

instance encodeJsonSetCustomer :: EncodeJson SetCustomer where
  encodeJson (SetCustomer {name,address})
    =  "name" := name
    ~> "address" := address
    ~> jsonEmptyObject

instance decodeJsonSetCustomer :: DecodeJson SetCustomer where
  decodeJson json = do
    o <- decodeJson json
    name <- o .? "name"
    address <- o .? "address"
    pure (SetCustomer {name,address})

newtype CustomerValid = CustomerValid
  { name    :: Name
  , address :: USAAddress
  }

derive instance genericCustomerValid :: Generic CustomerValid

instance eqCustomerValid :: Eq CustomerValid where
  eq = gEq

instance showCustomerValid :: Show CustomerValid where
  show = gShow

instance arbitraryCustomerValid :: Arbitrary CustomerValid where
  arbitrary = do
    name <- arbitrary
    address <- arbitrary
    pure (CustomerValid {name,address})

instance encodeJsonCustomerValid :: EncodeJson CustomerValid where
  encodeJson (CustomerValid {name,address})
    =  "name" := name
    ~> "address" := address
    ~> jsonEmptyObject

instance decodeJsonCustomerValid :: DecodeJson CustomerValid where
  decodeJson json = do
    o <- decodeJson json
    name <- o .? "name"
    address <- o .? "address"
    pure (CustomerValid {name,address})


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


newtype SubmitReview = SubmitReview
  { order     :: StoredOrderId
  , rating    :: Rating
  , heading   :: String
  , body      :: MarkdownText
  , images    :: Array ImageSource
  }


derive instance genericSubmitReview :: Generic SubmitReview

instance eqSubmitReview :: Eq SubmitReview where
  eq = gEq

instance showSubmitReview :: Show SubmitReview where
  show = gShow

instance arbitrarySubmitReview :: Arbitrary SubmitReview where
  arbitrary = do
    order <- arbitrary
    rating <- arbitrary
    heading <- arbitrary
    body <- arbitrary
    images <- arbitrary
    pure (SubmitReview {order,rating,heading,body,images})

instance encodeJsonSubmitReview :: EncodeJson SubmitReview where
  encodeJson (SubmitReview {order,rating,heading,body,images})
    =  "order" := order
    ~> "rating" := rating
    ~> "heading" := heading
    ~> "body" := body
    ~> "images" := images
    ~> jsonEmptyObject

instance decodeJsonSubmitReview :: DecodeJson SubmitReview where
  decodeJson json = do
    o <- decodeJson json
    order <- o .? "order"
    rating <- o .? "rating"
    heading <- o .? "heading"
    body <- o .? "body"
    images <- o .? "images"
    pure (SubmitReview {order,rating,heading,body,images})

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


-- * Browse

newtype BrowseMenu = BrowseMenu
  { chef :: Permalink
  , deadline :: Date
  }

derive instance genericBrowseMenu :: Generic BrowseMenu

instance eqBrowseMenu :: Eq BrowseMenu where
  eq = gEq

instance showBrowseMenu :: Show BrowseMenu where
  show = gShow


instance arbitraryBrowseMenu :: Arbitrary BrowseMenu where
  arbitrary = do
    chef <- arbitrary
    JSONDate deadline <- arbitrary
    pure (BrowseMenu {chef,deadline})


instance encodeJsonBrowseMenu :: EncodeJson BrowseMenu where
  encodeJson (BrowseMenu {chef,deadline})
    =  "chef" := chef
    ~> "deadline" := JSONDate deadline
    ~> jsonEmptyObject

instance decodeJsonBrowseMenu :: DecodeJson BrowseMenu where
  decodeJson json = do
    o <- decodeJson json
    chef <- o .? "chef"
    JSONDate deadline <- o .? "deadline"
    pure (BrowseMenu {chef,deadline})


newtype BrowseMeal = BrowseMeal
  { chef :: Permalink
  , deadline :: Date
  , meal :: Permalink
  }

derive instance genericBrowseMeal :: Generic BrowseMeal

instance eqBrowseMeal :: Eq BrowseMeal where
  eq = gEq

instance showBrowseMeal :: Show BrowseMeal where
  show = gShow


instance arbitraryBrowseMeal :: Arbitrary BrowseMeal where
  arbitrary = do
    chef <- arbitrary
    JSONDate deadline <- arbitrary
    meal <- arbitrary
    pure (BrowseMeal {chef,deadline,meal})

instance encodeJsonBrowseMeal :: EncodeJson BrowseMeal where
  encodeJson (BrowseMeal {chef,deadline,meal})
    =  "chef" := chef
    ~> "deadline" := JSONDate deadline
    ~> "meal" := meal
    ~> jsonEmptyObject

instance decodeJsonBrowseMeal :: DecodeJson BrowseMeal where
  decodeJson json = do
    o <- decodeJson json
    chef <- o .? "chef"
    JSONDate deadline <- o .? "deadline"
    meal <- o .? "meal"
    pure (BrowseMeal {chef,deadline,meal})


newtype AddToCart = AddToCart
  { chef :: Permalink
  , deadline :: Date
  , meal :: Permalink
  , volume :: Int
  }

derive instance genericAddToCart :: Generic AddToCart

instance eqAddToCart :: Eq AddToCart where
  eq = gEq

instance showAddToCart :: Show AddToCart where
  show = gShow

instance arbitraryAddToCart :: Arbitrary AddToCart where
  arbitrary = do
    chef <- arbitrary
    JSONDate deadline <- arbitrary
    meal <- arbitrary
    volume <- arbitrary
    pure (AddToCart {chef,deadline,meal,volume})

instance encodeJsonAddToCart :: EncodeJson AddToCart where
  encodeJson (AddToCart {chef,deadline,meal,volume})
    =  "chef" := chef
    ~> "deadline" := JSONDate deadline
    ~> "meal" := meal
    ~> "volume" := volume
    ~> jsonEmptyObject

instance decodeJsonAddToCart :: DecodeJson AddToCart where
  decodeJson json = do
    o <- decodeJson json
    chef <- o .? "chef"
    JSONDate deadline <- o .? "deadline"
    meal <- o .? "meal"
    volume <- o .? "volume"
    pure (AddToCart {chef,deadline,meal,volume})




-- * Errors


data CustomerExists a
  = CustomerDoesntExist
  | CustomerExists a

derive instance genericCustomerExists :: Generic a => Generic (CustomerExists a)

instance arbitraryCustomerExists :: Arbitrary a => Arbitrary (CustomerExists a) where
  arbitrary = oneOf $ NonEmpty
    ( pure CustomerDoesntExist
    )
    [ CustomerExists <$> arbitrary
    ]

instance eqCustomerExists :: Generic a => Eq (CustomerExists a) where
  eq = gEq

instance showCustomerExists :: Generic a => Show (CustomerExists a) where
  show = gShow

instance encodeJsonCustomerExists :: EncodeJson a => EncodeJson (CustomerExists a) where
  encodeJson x = case x of
    CustomerDoesntExist -> encodeJson "customerDoesntExist"
    CustomerExists y
      -> "customerExists"
      := y
      ~> jsonEmptyObject

instance decodeJsonCustomerExists :: DecodeJson a => DecodeJson (CustomerExists a) where
  decodeJson json = do
    let empty = do
          s <- decodeJson json
          if s == "customerDoesntExist"
             then pure CustomerDoesntExist
             else fail "Not a CustomerExists"
        has = do
          o <- decodeJson json
          CustomerExists <$> o .? "customerExists"
    empty <|> has



data CustomerUnique a
  = CustomerNotUnique
  | CustomerUnique a

derive instance genericCustomerUnique :: Generic a => Generic (CustomerUnique a)

instance arbitraryCustomerUnique :: Arbitrary a => Arbitrary (CustomerUnique a) where
  arbitrary = oneOf $ NonEmpty
    ( pure CustomerNotUnique
    )
    [ CustomerUnique <$> arbitrary
    ]

instance eqCustomerUnique :: Generic a => Eq (CustomerUnique a) where
  eq = gEq

instance showCustomerUnique :: Generic a => Show (CustomerUnique a) where
  show = gShow

instance encodeJsonCustomerUnique :: EncodeJson a => EncodeJson (CustomerUnique a) where
  encodeJson x = case x of
    CustomerNotUnique -> encodeJson "customerNotUnique"
    CustomerUnique y
      -> "customerUnique"
      := y
      ~> jsonEmptyObject

instance decodeJsonCustomerUnique :: DecodeJson a => DecodeJson (CustomerUnique a) where
  decodeJson json = do
    let empty = do
          s <- decodeJson json
          if s == "customerNotUnique"
             then pure CustomerNotUnique
             else fail "Not a CustomerUnique"
        has = do
          o <- decodeJson json
          CustomerUnique <$> o .? "customerUnique"
    empty <|> has


data OrderExists a
  = OrderDoesntExist
  | OrderExists a

derive instance genericOrderExists :: Generic a => Generic (OrderExists a)

instance arbitraryOrderExists :: Arbitrary a => Arbitrary (OrderExists a) where
  arbitrary = oneOf $ NonEmpty
    ( pure OrderDoesntExist
    )
    [ OrderExists <$> arbitrary
    ]

instance eqOrderExists :: Generic a => Eq (OrderExists a) where
  eq = gEq

instance showOrderExists :: Generic a => Show (OrderExists a) where
  show = gShow

instance encodeJsonOrderExists :: EncodeJson a => EncodeJson (OrderExists a) where
  encodeJson x = case x of
    OrderDoesntExist -> encodeJson "orderDoesntExist"
    OrderExists y
      -> "orderExists"
      := y
      ~> jsonEmptyObject

instance decodeJsonOrderExists :: DecodeJson a => DecodeJson (OrderExists a) where
  decodeJson json = do
    let empty = do
          s <- decodeJson json
          if s == "orderDoesntExist"
             then pure OrderDoesntExist
             else fail "Not a OrderExists"
        has = do
          o <- decodeJson json
          OrderExists <$> o .? "orderExists"
    empty <|> has


data ReviewExists a
  = ReviewDoesntExist
  | ReviewExists a

derive instance genericReviewExists :: Generic a => Generic (ReviewExists a)

instance arbitraryReviewExists :: Arbitrary a => Arbitrary (ReviewExists a) where
  arbitrary = oneOf $ NonEmpty
    ( pure ReviewDoesntExist
    )
    [ ReviewExists <$> arbitrary
    ]

instance eqReviewExists :: Generic a => Eq (ReviewExists a) where
  eq = gEq

instance showReviewExists :: Generic a => Show (ReviewExists a) where
  show = gShow

instance encodeJsonReviewExists :: EncodeJson a => EncodeJson (ReviewExists a) where
  encodeJson x = case x of
    ReviewDoesntExist -> encodeJson "reviewDoesntExist"
    ReviewExists y
      -> "reviewExists"
      := y
      ~> jsonEmptyObject

instance decodeJsonReviewExists :: DecodeJson a => DecodeJson (ReviewExists a) where
  decodeJson json = do
    let empty = do
          s <- decodeJson json
          if s == "reviewDoesntExist"
             then pure ReviewDoesntExist
             else fail "Not a ReviewExists"
        has = do
          o <- decodeJson json
          ReviewExists <$> o .? "reviewExists"
    empty <|> has


data RatingExists a
  = RatingDoesntExist
  | RatingExists a

derive instance genericRatingExists :: Generic a => Generic (RatingExists a)

instance arbitraryRatingExists :: Arbitrary a => Arbitrary (RatingExists a) where
  arbitrary = oneOf $ NonEmpty
    ( pure RatingDoesntExist
    )
    [ RatingExists <$> arbitrary
    ]

instance eqRatingExists :: Generic a => Eq (RatingExists a) where
  eq = gEq

instance showRatingExists :: Generic a => Show (RatingExists a) where
  show = gShow

instance encodeJsonRatingExists :: EncodeJson a => EncodeJson (RatingExists a) where
  encodeJson x = case x of
    RatingDoesntExist -> encodeJson "ratingDoesntExist"
    RatingExists y
      -> "ratingExists"
      := y
      ~> jsonEmptyObject

instance decodeJsonRatingExists :: DecodeJson a => DecodeJson (RatingExists a) where
  decodeJson json = do
    let empty = do
          s <- decodeJson json
          if s == "ratingDoesntExist"
             then pure RatingDoesntExist
             else fail "Not a RatingExists"
        has = do
          o <- decodeJson json
          RatingExists <$> o .? "ratingExists"
    empty <|> has


data MealExists a
  = MealDoesntExist
  | MealExists a

derive instance genericMealExists :: Generic a => Generic (MealExists a)

instance arbitraryMealExists :: Arbitrary a => Arbitrary (MealExists a) where
  arbitrary = oneOf $ NonEmpty
    ( pure MealDoesntExist
    )
    [ MealExists <$> arbitrary
    ]

instance eqMealExists :: Generic a => Eq (MealExists a) where
  eq = gEq

instance showMealExists :: Generic a => Show (MealExists a) where
  show = gShow

instance encodeJsonMealExists :: EncodeJson a => EncodeJson (MealExists a) where
  encodeJson x = case x of
    MealDoesntExist -> encodeJson "mealDoesntExist"
    MealExists y
      -> "mealExists"
      := y
      ~> jsonEmptyObject

instance decodeJsonMealExists :: DecodeJson a => DecodeJson (MealExists a) where
  decodeJson json = do
    let empty = do
          s <- decodeJson json
          if s == "mealDoesntExist"
             then pure MealDoesntExist
             else fail "Not a MealExists"
        has = do
          o <- decodeJson json
          MealExists <$> o .? "mealExists"
    empty <|> has


data MenuExists a
  = MenuDoesntExist
  | MenuExists a

derive instance genericMenuExists :: Generic a => Generic (MenuExists a)

instance arbitraryMenuExists :: Arbitrary a => Arbitrary (MenuExists a) where
  arbitrary = oneOf $ NonEmpty
    ( pure MenuDoesntExist
    )
    [ MenuExists <$> arbitrary
    ]

instance eqMenuExists :: Generic a => Eq (MenuExists a) where
  eq = gEq

instance showMenuExists :: Generic a => Show (MenuExists a) where
  show = gShow

instance encodeJsonMenuExists :: EncodeJson a => EncodeJson (MenuExists a) where
  encodeJson x = case x of
    MenuDoesntExist -> encodeJson "menuDoesntExist"
    MenuExists y
      -> "menuExists"
      := y
      ~> jsonEmptyObject

instance decodeJsonMenuExists :: DecodeJson a => DecodeJson (MenuExists a) where
  decodeJson json = do
    let empty = do
          s <- decodeJson json
          if s == "menuDoesntExist"
             then pure MenuDoesntExist
             else fail "Not a MenuExists"
        has = do
          o <- decodeJson json
          MenuExists <$> o .? "menuExists"
    empty <|> has


data MealUnique a
  = MealNotUnique
  | MealUnique a

derive instance genericMealUnique :: Generic a => Generic (MealUnique a)

instance arbitraryMealUnique :: Arbitrary a => Arbitrary (MealUnique a) where
  arbitrary = oneOf $ NonEmpty
    ( pure MealNotUnique
    )
    [ MealUnique <$> arbitrary
    ]

instance eqMealUnique :: Generic a => Eq (MealUnique a) where
  eq = gEq

instance showMealUnique :: Generic a => Show (MealUnique a) where
  show = gShow

instance encodeJsonMealUnique :: EncodeJson a => EncodeJson (MealUnique a) where
  encodeJson x = case x of
    MealNotUnique -> encodeJson "mealNotUnique"
    MealUnique y
      -> "mealUnique"
      := y
      ~> jsonEmptyObject

instance decodeJsonMealUnique :: DecodeJson a => DecodeJson (MealUnique a) where
  decodeJson json = do
    let empty = do
          s <- decodeJson json
          if s == "mealNotUnique"
             then pure MealNotUnique
             else fail "Not a MealUnique"
        has = do
          o <- decodeJson json
          MealUnique <$> o .? "mealUnique"
    empty <|> has

data MenuUnique a
  = MenuNotUnique
  | MenuUnique a

derive instance genericMenuUnique :: Generic a => Generic (MenuUnique a)

instance arbitraryMenuUnique :: Arbitrary a => Arbitrary (MenuUnique a) where
  arbitrary = oneOf $ NonEmpty
    ( pure MenuNotUnique
    )
    [ MenuUnique <$> arbitrary
    ]

instance eqMenuUnique :: Generic a => Eq (MenuUnique a) where
  eq = gEq

instance showMenuUnique :: Generic a => Show (MenuUnique a) where
  show = gShow

instance encodeJsonMenuUnique :: EncodeJson a => EncodeJson (MenuUnique a) where
  encodeJson x = case x of
    MenuNotUnique -> encodeJson "menuNotUnique"
    MenuUnique y
      -> "menuUnique"
      := y
      ~> jsonEmptyObject

instance decodeJsonMenuUnique :: DecodeJson a => DecodeJson (MenuUnique a) where
  decodeJson json = do
    let empty = do
          s <- decodeJson json
          if s == "menuNotUnique"
             then pure MenuNotUnique
             else fail "Not a MenuUnique"
        has = do
          o <- decodeJson json
          MenuUnique <$> o .? "menuUnique"
    empty <|> has


data MenuPublished a
  = MenuNotPublished
  | MenuPublished a

derive instance genericMenuPublished :: Generic a => Generic (MenuPublished a)

instance arbitraryMenuPublished :: Arbitrary a => Arbitrary (MenuPublished a) where
  arbitrary = oneOf $ NonEmpty
    ( pure MenuNotPublished
    )
    [ MenuPublished <$> arbitrary
    ]

instance eqMenuPublished :: Generic a => Eq (MenuPublished a) where
  eq = gEq

instance showMenuPublished :: Generic a => Show (MenuPublished a) where
  show = gShow

instance encodeJsonMenuPublished :: EncodeJson a => EncodeJson (MenuPublished a) where
  encodeJson x = case x of
    MenuNotPublished -> encodeJson "menuNotPublished"
    MenuPublished y
      -> "menuPublished"
      := y
      ~> jsonEmptyObject

instance decodeJsonMenuPublished :: DecodeJson a => DecodeJson (MenuPublished a) where
  decodeJson json = do
    let empty = do
          s <- decodeJson json
          if s == "menuNotPublished"
             then pure MenuNotPublished
             else fail "Not a MenuPublished"
        has = do
          o <- decodeJson json
          MenuPublished <$> o .? "menuPublished"
    empty <|> has
