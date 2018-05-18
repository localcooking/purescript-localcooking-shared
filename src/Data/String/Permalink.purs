module Data.String.Permalink where

import Prelude
import Data.Generic (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson)


newtype Permalink = Permalink String

derive instance genericPermalink :: Generic Permalink
derive newtype instance eqPermalink :: Eq Permalink
derive newtype instance ordPermalink :: Ord Permalink
derive newtype instance showPermalink :: Show Permalink
derive newtype instance encodeJsonPermalink :: EncodeJson Permalink
derive newtype instance decodeJsonPermalink :: DecodeJson Permalink
