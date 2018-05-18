module LocalCooking.Common.AccessToken.Email where

import LocalCooking.Common.AccessToken (AccessToken)

import Prelude
import Data.Generic (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Newtype (class Newtype)
import Test.QuickCheck (class Arbitrary)


newtype EmailToken = EmailToken AccessToken

derive instance genericEmailToken :: Generic EmailToken
derive instance newtypeEmailToken :: Newtype EmailToken _
derive newtype instance arbitraryEmailToken :: Arbitrary EmailToken
derive newtype instance eqEmailToken :: Eq EmailToken
derive newtype instance showEmailToken :: Show EmailToken
derive newtype instance encodeJsonEmailToken :: EncodeJson EmailToken
derive newtype instance decodeJsonEmailToken :: DecodeJson EmailToken
