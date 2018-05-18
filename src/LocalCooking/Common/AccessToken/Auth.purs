module LocalCooking.Common.AccessToken.Auth where

import LocalCooking.Common.AccessToken (AccessToken)

import Prelude
import Data.Generic (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Newtype (class Newtype)
import Test.QuickCheck (class Arbitrary)


newtype AuthToken = AuthToken AccessToken

derive instance genericAuthToken :: Generic AuthToken
derive instance newtypeAuthToken :: Newtype AuthToken _
derive newtype instance arbitraryAuthToken :: Arbitrary AuthToken
derive newtype instance eqAuthToken :: Eq AuthToken
derive newtype instance showAuthToken :: Show AuthToken
derive newtype instance encodeJsonAuthToken :: EncodeJson AuthToken
derive newtype instance decodeJsonAuthToken :: DecodeJson AuthToken
