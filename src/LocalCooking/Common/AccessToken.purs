module LocalCooking.Common.AccessToken where

import Prelude
import Data.Generic (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.String.Yarn (class IsString)
import Test.QuickCheck (class Arbitrary)


newtype AccessToken = AccessToken String

derive instance genericAccessToken :: Generic AccessToken
derive newtype instance arbitraryAccessToken :: Arbitrary AccessToken
derive newtype instance eqAccessToken :: Eq AccessToken
derive newtype instance showAccessToken :: Show AccessToken
derive newtype instance encodeJsonAccessToken :: EncodeJson AccessToken
derive newtype instance decodeJsonAccessToken :: DecodeJson AccessToken
derive newtype instance isStringAccessToken :: IsString AccessToken
