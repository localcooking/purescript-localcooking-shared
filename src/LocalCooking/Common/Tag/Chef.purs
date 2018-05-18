module LocalCooking.Common.Tag.Chef where

import LocalCooking.Common.Tag (Tag)

import Prelude
import Data.Generic (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Newtype (class Newtype)


newtype ChefTag = ChefTag Tag

derive instance genericChefTag :: Generic ChefTag
derive instance newtypeChefTag :: Newtype ChefTag _
derive newtype instance eqChefTag :: Eq ChefTag
derive newtype instance ordChefTag :: Ord ChefTag
derive newtype instance showChefTag :: Show ChefTag
derive newtype instance encodeJsonChefTag :: EncodeJson ChefTag
derive newtype instance decodeJsonChefTag :: DecodeJson ChefTag
