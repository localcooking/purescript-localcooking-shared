module Google.ReCaptcha where

import Prelude (class Eq, class Show)
import Data.Generic (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Test.QuickCheck (class Arbitrary)


-- | Response from verfiying reCaptcha value
newtype ReCaptchaResponse = ReCaptchaResponse String

derive instance genericReCaptchaResponse :: Generic ReCaptchaResponse
derive newtype instance arbitraryReCaptchaResponse :: Arbitrary ReCaptchaResponse
derive newtype instance eqReCaptchaResponse :: Eq ReCaptchaResponse
derive newtype instance showReCaptchaResponse :: Show ReCaptchaResponse
derive newtype instance encodeJsonReCaptchaResponse :: EncodeJson ReCaptchaResponse
derive newtype instance decodeJsonReCaptchaResponse :: DecodeJson ReCaptchaResponse


-- | Public key for google app
newtype ReCaptchaSiteKey = ReCaptchaSiteKey String

derive instance genericReCaptchaSiteKey :: Generic ReCaptchaSiteKey
derive newtype instance arbitraryReCaptchaSiteKey :: Arbitrary ReCaptchaSiteKey
derive newtype instance eqReCaptchaSiteKey :: Eq ReCaptchaSiteKey
derive newtype instance showReCaptchaSiteKey :: Show ReCaptchaSiteKey
derive newtype instance encodeJsonReCaptchaSiteKey :: EncodeJson ReCaptchaSiteKey
derive newtype instance decodeJsonReCaptchaSiteKey :: DecodeJson ReCaptchaSiteKey
