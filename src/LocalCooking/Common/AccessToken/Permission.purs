module LocalCooking.Common.AccessToken.Permission where

import LocalCooking.Common.AccessToken (AccessToken)

import Prelude
import Data.Generic (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Newtype (class Newtype)
import Data.String.Yarn (class IsString)
import Test.QuickCheck (class Arbitrary)


newtype PermissionToken = PermissionToken AccessToken

derive instance genericPermissionToken :: Generic PermissionToken
derive instance newtypePermissionToken :: Newtype PermissionToken _
derive newtype instance arbitraryPermissionToken :: Arbitrary PermissionToken
derive newtype instance eqPermissionToken :: Eq PermissionToken
derive newtype instance showPermissionToken :: Show PermissionToken
derive newtype instance encodeJsonPermissionToken :: EncodeJson PermissionToken
derive newtype instance decodeJsonPermissionToken :: DecodeJson PermissionToken
derive newtype instance isStringPermissionToken :: IsString PermissionToken
