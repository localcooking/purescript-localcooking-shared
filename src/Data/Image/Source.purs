module Data.Image.Source where

import Prelude
import Data.Generic (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson)


newtype ImageSource = ImageSource String

derive instance genericImageSource :: Generic ImageSource
derive newtype instance eqImageSource :: Eq ImageSource
derive newtype instance ordImageSource :: Ord ImageSource
derive newtype instance showImageSource :: Show ImageSource
derive newtype instance encodeJsonImageSource :: EncodeJson ImageSource
derive newtype instance decodeJsonImageSource :: DecodeJson ImageSource
