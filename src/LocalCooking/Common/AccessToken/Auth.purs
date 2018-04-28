module LocalCooking.Common.AccessToken.Auth where

import LocalCooking.Common.AccessToken (AccessToken)

import Prelude
import Data.Generic (class Generic, gEq, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Data.Newtype (class Newtype)
import Test.QuickCheck (class Arbitrary, arbitrary)


newtype AuthToken = AuthToken AccessToken

derive instance genericAuthToken :: Generic AuthToken

derive instance newtypeAuthToken :: Newtype AuthToken _

instance arbitraryAuthToken :: Arbitrary AuthToken where
  arbitrary = AuthToken <$> arbitrary

instance eqAuthToken :: Eq AuthToken where
  eq = gEq

instance showAuthToken :: Show AuthToken where
  show = gShow

instance encodeJsonAuthToken :: EncodeJson AuthToken where
  encodeJson (AuthToken x) = encodeJson x

instance decodeJsonAuthToken :: DecodeJson AuthToken where
  decodeJson json = AuthToken <$> decodeJson json
