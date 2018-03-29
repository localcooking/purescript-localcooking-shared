module LocalCooking.Common.AuthToken where

import Prelude
import Data.Generic (class Generic, gEq, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)


newtype AuthToken = AuthToken String

derive instance genericAuthToken :: Generic AuthToken

instance eqAuthToken :: Eq AuthToken where
  eq = gEq

instance showAuthToken :: Show AuthToken where
  show = gShow

instance encodeJsonAuthToken :: EncodeJson AuthToken where
  encodeJson (AuthToken x) = encodeJson x

instance decodeJsonAuthToken :: DecodeJson AuthToken where
  decodeJson json = AuthToken <$> decodeJson json
