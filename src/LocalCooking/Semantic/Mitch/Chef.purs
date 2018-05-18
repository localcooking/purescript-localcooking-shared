module LocalCooking.Semantic.Mitch.Chef where

import LocalCooking.Common.Rating (Rating)
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Common.User.Name (Name)

import Prelude
import Data.Image.Source (ImageSource)
import Data.Generic (class Generic, gShow, gEq)
import Data.Argonaut (class EncodeJson, class DecodeJson, (~>), (:=), decodeJson, (.?), jsonEmptyObject)


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

