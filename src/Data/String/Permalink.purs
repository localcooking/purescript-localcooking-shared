module Data.String.Permalink where

import Prelude
import Data.Generic (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.String.Yarn (class IsString)
import Test.QuickCheck (class Arbitrary)


newtype Permalink = Permalink String

derive instance genericPermalink :: Generic Permalink
derive newtype instance arbitraryPermalink :: Arbitrary Permalink
derive newtype instance eqPermalink :: Eq Permalink
derive newtype instance ordPermalink :: Ord Permalink
derive newtype instance showPermalink :: Show Permalink
derive newtype instance encodeJsonPermalink :: EncodeJson Permalink
derive newtype instance decodeJsonPermalink :: DecodeJson Permalink
derive newtype instance isStringPermalink :: IsString Permalink
