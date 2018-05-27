module Facebook.Types where

import Prelude
import Data.Generic (class Generic, gEq, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, jsonEmptyObject, (~>), (:=), (.?), fail)
import Data.NonEmpty (NonEmpty (..))
import Control.Alternative ((<|>))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)


newtype FacebookUserId = FacebookUserId String

derive instance genericFacebookUserId :: Generic FacebookUserId
derive newtype instance eqFacebookUserId :: Eq FacebookUserId
derive newtype instance showFacebookUserId :: Show FacebookUserId
derive newtype instance encodeJsonFacebookUserId :: EncodeJson FacebookUserId
derive newtype instance decodeJsonFacebookUserId :: DecodeJson FacebookUserId
derive newtype instance arbitraryFacebookUserId :: Arbitrary FacebookUserId



data FacebookLoginReturnError
  = FacebookLoginVerifyParseFailure String
  | FacebookLoginUserDetailsParseFailure String
  | FacebookLoginGetTokenError' String String Int String

derive instance genericFacebookLoginReturnError :: Generic FacebookLoginReturnError

instance eqFacebookLoginReturnError :: Eq FacebookLoginReturnError where
  eq = gEq

instance arbitraryFacebookLoginReturnError :: Arbitrary FacebookLoginReturnError where
  arbitrary = oneOf $ NonEmpty
    ( FacebookLoginVerifyParseFailure <$> arbitrary
    )
    [ FacebookLoginUserDetailsParseFailure <$> arbitrary
    , FacebookLoginGetTokenError' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    ]

instance encodeJsonFacebookLoginReturnError :: EncodeJson FacebookLoginReturnError where
  encodeJson x = case x of
    FacebookLoginVerifyParseFailure x
      -> "verifyParseFailure" := x
      ~> jsonEmptyObject
    FacebookLoginUserDetailsParseFailure x
      -> "userDetailsParseFailure" := x
      ~> jsonEmptyObject
    FacebookLoginGetTokenError' a b c d
      -> "getTokenError" :=
         ( "message" := a
         ~> "type" := b
         ~> "code" := c
         ~> "fbTraceId" := d
         ~> jsonEmptyObject
         )
      ~> jsonEmptyObject

instance decodeJsonFacebookLoginReturnError :: DecodeJson FacebookLoginReturnError where
  decodeJson json = do
    o <- decodeJson json
    let verify = do
          FacebookLoginVerifyParseFailure <$> o .? "verifyParseFailure"
        userDetails = do
          FacebookLoginUserDetailsParseFailure <$> o .? "userDetailsParseFailure"
        getToken = do
          o' <- o .? "getTokenError"
          FacebookLoginGetTokenError'
            <$> o' .? "message"
            <*> o' .? "type"
            <*> o' .? "code"
            <*> o' .? "fbTraceId"
    verify <|> userDetails <|> getToken


-- * Credentials


newtype FacebookClientId = FacebookClientId String

derive instance genericFacebookClientId :: Generic FacebookClientId
derive newtype instance eqFacebookClientId :: Eq FacebookClientId
derive newtype instance showFacebookClientId :: Show FacebookClientId
derive newtype instance encodeJsonFacebookClientId :: EncodeJson FacebookClientId
derive newtype instance decodeJsonFacebookClientId :: DecodeJson FacebookClientId
derive newtype instance arbitraryFacebookClientId :: Arbitrary FacebookClientId
