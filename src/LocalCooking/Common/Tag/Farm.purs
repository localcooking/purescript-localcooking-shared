module LocalCooking.Common.Tag.Farm where

import LocalCooking.Common.Tag (Tag)

import Prelude
import Data.Generic (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Newtype (class Newtype)
import Data.String.Yarn (class IsString)
import Test.QuickCheck (class Arbitrary)


newtype FarmTag = FarmTag Tag

derive instance genericFarmTag :: Generic FarmTag
derive instance newtypeFarmTag :: Newtype FarmTag _
derive newtype instance arbitraryFarmTag :: Arbitrary FarmTag
derive newtype instance eqFarmTag :: Eq FarmTag
derive newtype instance ordFarmTag :: Ord FarmTag
derive newtype instance showFarmTag :: Show FarmTag
derive newtype instance encodeJsonFarmTag :: EncodeJson FarmTag
derive newtype instance decodeJsonFarmTag :: DecodeJson FarmTag
derive newtype instance isStringFarmTag :: IsString FarmTag
