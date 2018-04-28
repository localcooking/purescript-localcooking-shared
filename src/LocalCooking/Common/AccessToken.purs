module LocalCooking.Common.AccessToken where

import Prelude
import Data.Generic (class Generic, gEq, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Test.QuickCheck (class Arbitrary, arbitrary)


newtype AccessToken = AccessToken String

derive instance genericAccessToken :: Generic AccessToken

instance arbitraryAccessToken :: Arbitrary AccessToken where
  arbitrary = AccessToken <$> arbitrary

instance eqAccessToken :: Eq AccessToken where
  eq = gEq

instance showAccessToken :: Show AccessToken where
  show = gShow

instance encodeJsonAccessToken :: EncodeJson AccessToken where
  encodeJson (AccessToken x) = encodeJson x

instance decodeJsonAccessToken :: DecodeJson AccessToken where
  decodeJson json = AccessToken <$> decodeJson json
