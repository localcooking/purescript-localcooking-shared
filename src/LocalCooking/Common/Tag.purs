module LocalCooking.Common.Tag where

import Prelude
import Data.Generic (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.String.Yarn (class IsString)
import Test.QuickCheck (class Arbitrary)


newtype Tag = Tag String

derive instance genericTag :: Generic Tag
derive newtype instance arbitraryTag :: Arbitrary Tag
derive newtype instance eqTag :: Eq Tag
derive newtype instance ordTag :: Ord Tag
derive newtype instance showTag :: Show Tag
derive newtype instance encodeJsonTag :: EncodeJson Tag
derive newtype instance decodeJsonTag :: DecodeJson Tag
derive newtype instance isStringTag :: IsString Tag
