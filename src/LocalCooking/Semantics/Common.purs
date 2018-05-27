module LocalCooking.Semantics.Common where

import LocalCooking.Database.Schema (StoredUserId)
import LocalCooking.Common.User.Role (UserRole)
import Facebook.Types (FacebookUserId)

import Prelude
import Data.DateTime (DateTime)
import Data.Maybe (Maybe)
import Data.Generic (class Generic, gEq, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, fail, (:=), (~>), jsonEmptyObject, (.?))
import Text.Email.Validate (EmailAddress)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)


newtype SocialLoginForm = SocialLoginForm
  { fb :: Maybe FacebookUserId
  }

derive instance genericSocialLoginForm :: Generic SocialLoginForm

instance arbitrarySocialLoginForm :: Arbitrary SocialLoginForm where
  arbitrary = do
    fb <- arbitrary
    pure (SocialLoginForm {fb})

instance eqSocialLoginForm :: Eq SocialLoginForm where
  eq = gEq

instance showSocialLoginForm :: Show SocialLoginForm where
  show = gShow

instance encodeJsonSocialLoginForm :: EncodeJson SocialLoginForm where
  encodeJson (SocialLoginForm {fb})
    =  "fb" := fb
    ~> jsonEmptyObject

instance decodeJsonSocialLoginForm :: DecodeJson SocialLoginForm where
  decodeJson json = do
    o <- decodeJson json
    fb <- o .? "fb"
    pure (SocialLoginForm {fb})


newtype User = User
  { id             :: StoredUserId
  , created        :: DateTime -- FIXME SHIT, isomorphic to UTCTime?
  , email          :: EmailAddress
  , social         :: SocialLoginForm
  , emailConfirmed :: Boolean
  , roles          :: Array UserRole
  }

derive instance genericUser :: Generic User

instance arbitraryUser :: Arbitrary User where
  arbitrary = do
    id <- arbitrary
    created <- arbitrary
    email <- arbitrary
    social <- arbitrary
    emailConfirmed <- arbitrary
    roles <- arbitrary
    pure (User {id,created,email,social,emailConfirmed,roles})

instance eqUser :: Eq User where
  eq = gEq

instance showUser :: Show User where
  show = gShow

instance encodeJsonUser :: EncodeJson User where
  encodeJson (User {id,created,email,social,emailConfirmed,roles})
    =  "id" := id
    ~> "created" := created
    ~> "email" := email
    ~> "social" := social
    ~> "emailConfirmed" := emailConfirmed
    ~> "roles" := roles
    ~> jsonEmptyObject

instance decodeJsonUser :: DecodeJson User where
  decodeJson json = do
    o <- decodeJson json
    id <- o .? "id"
    created <- o .? "created"
    email <- o .? "email"
    social <- o .? "social"
    emailConfirmed <- o .? "emailConfirmed"
    roles <- o .? "roles"
    pure (User {id,created,email,social,emailConfirmed,roles})
