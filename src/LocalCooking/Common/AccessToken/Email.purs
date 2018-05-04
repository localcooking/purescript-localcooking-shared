module LocalCooking.Common.AccessToken.Email where

import LocalCooking.Common.AccessToken (AccessToken)

import Prelude
import Data.Generic (class Generic, gEq, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Data.Newtype (class Newtype)
import Test.QuickCheck (class Arbitrary, arbitrary)


newtype EmailToken = EmailToken AccessToken

derive instance genericEmailToken :: Generic EmailToken

derive instance newtypeEmailToken :: Newtype EmailToken _

instance arbitraryEmailToken :: Arbitrary EmailToken where
  arbitrary = EmailToken <$> arbitrary

instance eqEmailToken :: Eq EmailToken where
  eq = gEq

instance showEmailToken :: Show EmailToken where
  show = gShow

instance encodeJsonEmailToken :: EncodeJson EmailToken where
  encodeJson (EmailToken x) = encodeJson x

instance decodeJsonEmailToken :: DecodeJson EmailToken where
  decodeJson json = EmailToken <$> decodeJson json
