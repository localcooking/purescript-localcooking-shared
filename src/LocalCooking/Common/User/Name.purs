module LocalCooking.Common.User.Name where

import Prelude
import Data.Generic (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.String.Yarn (words) as String
import Test.QuickCheck (class Arbitrary)


newtype Name = Name (Array String)

derive instance genericName :: Generic Name
derive newtype instance arbitraryName :: Arbitrary Name
derive newtype instance eqName :: Eq Name
derive newtype instance ordName :: Ord Name
derive newtype instance showName :: Show Name
derive newtype instance encodeJsonName :: EncodeJson Name
derive newtype instance decodeJsonName :: DecodeJson Name


name :: String -> Name
name = Name <<< String.words
