module LocalCooking.Common.AccessToken.Permission where

import LocalCooking.Common.AccessToken (AccessToken)

import Prelude
import Data.Generic (class Generic, gEq, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Data.Newtype (class Newtype)
import Test.QuickCheck (class Arbitrary, arbitrary)


newtype PermissionToken = PermissionToken AccessToken

derive instance genericPermissionToken :: Generic PermissionToken

derive instance newtypePermissionToken :: Newtype PermissionToken _

instance arbitraryPermissionToken :: Arbitrary PermissionToken where
  arbitrary = PermissionToken <$> arbitrary

instance eqPermissionToken :: Eq PermissionToken where
  eq = gEq

instance showPermissionToken :: Show PermissionToken where
  show = gShow

instance encodeJsonPermissionToken :: EncodeJson PermissionToken where
  encodeJson (PermissionToken x) = encodeJson x

instance decodeJsonPermissionToken :: DecodeJson PermissionToken where
  decodeJson json = PermissionToken <$> decodeJson json
