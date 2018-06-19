module LocalCooking.Common.Culture where

import Prelude
import Data.Generic (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Test.QuickCheck (class Arbitrary)


newtype Culture = Culture String

derive instance genericCulture :: Generic Culture
derive newtype instance arbitraryCulture :: Arbitrary Culture
derive newtype instance eqCulture :: Eq Culture
derive newtype instance ordCulture :: Ord Culture
derive newtype instance showCulture :: Show Culture
derive newtype instance encodeJsonCulture :: EncodeJson Culture
derive newtype instance decodeJsonCulture :: DecodeJson Culture
