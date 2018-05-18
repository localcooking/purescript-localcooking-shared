module LocalCooking.Common.Diet where

import Prelude
import Data.Generic (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Test.QuickCheck (class Arbitrary)


newtype Diet = Diet String

derive instance genericDiet :: Generic Diet
derive newtype instance arbitraryDiet :: Arbitrary Diet
derive newtype instance eqDiet :: Eq Diet
derive newtype instance ordDiet :: Ord Diet
derive newtype instance showDiet :: Show Diet
derive newtype instance encodeJsonDiet :: EncodeJson Diet
derive newtype instance decodeJsonDiet :: DecodeJson Diet
