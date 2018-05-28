module Google.ReCaptcha where

import Prelude ((<$>), class Eq, class Show)
import Data.Generic (class Generic, gEq, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Test.QuickCheck (class Arbitrary, arbitrary)


newtype ReCaptchaResponse = ReCaptchaResponse String

derive instance genericReCaptchaResponse :: Generic ReCaptchaResponse
derive newtype instance arbitraryReCaptchaResponse :: Arbitrary ReCaptchaResponse
derive newtype instance eqReCaptchaResponse :: Eq ReCaptchaResponse
derive newtype instance showReCaptchaResponse :: Show ReCaptchaResponse
derive newtype instance encodeJsonReCaptchaResponse :: EncodeJson ReCaptchaResponse
derive newtype instance decodeJsonReCaptchaResponse :: DecodeJson ReCaptchaResponse
