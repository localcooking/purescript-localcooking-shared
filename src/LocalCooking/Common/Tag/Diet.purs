module LocalCooking.Common.Tag.Diet where

import LocalCooking.Common.Tag (Tag)

import Prelude
import Data.Generic (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Newtype (class Newtype)
import Data.String.Yarn (class IsString)
import Test.QuickCheck (class Arbitrary)


newtype DietTag = DietTag Tag

derive instance genericDietTag :: Generic DietTag
derive instance newtypeDietTag :: Newtype DietTag _
derive newtype instance arbitraryDietTag :: Arbitrary DietTag
derive newtype instance eqDietTag :: Eq DietTag
derive newtype instance ordDietTag :: Ord DietTag
derive newtype instance showDietTag :: Show DietTag
derive newtype instance encodeJsonDietTag :: EncodeJson DietTag
derive newtype instance decodeJsonDietTag :: DecodeJson DietTag
derive newtype instance isStringDietTag :: IsString DietTag
