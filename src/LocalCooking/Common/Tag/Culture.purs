module LocalCooking.Common.Tag.Culture where

import LocalCooking.Common.Tag (Tag)

import Prelude
import Data.Generic (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Newtype (class Newtype)
import Data.String.Yarn (class IsString)
import Test.QuickCheck (class Arbitrary)


newtype CultureTag = CultureTag Tag

derive instance genericCultureTag :: Generic CultureTag
derive instance newtypeCultureTag :: Newtype CultureTag _
derive newtype instance arbitraryCultureTag :: Arbitrary CultureTag
derive newtype instance eqCultureTag :: Eq CultureTag
derive newtype instance ordCultureTag :: Ord CultureTag
derive newtype instance showCultureTag :: Show CultureTag
derive newtype instance encodeJsonCultureTag :: EncodeJson CultureTag
derive newtype instance decodeJsonCultureTag :: DecodeJson CultureTag
derive newtype instance isStringCultureTag :: IsString CultureTag
