module LocalCooking.Semantics.Admin where

import LocalCooking.Semantics.Common (User)
import LocalCooking.Common.User.Password (HashedPassword)

import Prelude
import Data.Maybe (Maybe)
import Data.Generic (class Generic, gEq, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, (:=), (~>), jsonEmptyObject, (.?))
import Text.Email.Validate (EmailAddress)
import Test.QuickCheck (class Arbitrary, arbitrary)



newtype SetUser = SetUser
  { user :: User
  , newPassword :: Maybe HashedPassword
  }

derive instance genericSetUser :: Generic SetUser

instance arbitrarySetUser :: Arbitrary SetUser where
  arbitrary = do
    user <- arbitrary
    newPassword <- arbitrary
    pure (SetUser {user,newPassword})

instance eqSetUser :: Eq SetUser where
  eq = gEq

instance showSetUser :: Show SetUser where
  show = gShow

instance encodeJsonSetUser :: EncodeJson SetUser where
  encodeJson (SetUser {user,newPassword})
    =  "user" := user
    ~> "newPassword" := newPassword
    ~> jsonEmptyObject

instance decodeJsonSetUser :: DecodeJson SetUser where
  decodeJson json = do
    o <- decodeJson json
    user <- o .? "user"
    newPassword <- o .? "newPassword"
    pure (SetUser {user,newPassword})


newtype NewUser = NewUser
  { email     :: EmailAddress
  , password  :: HashedPassword
  }

derive instance genericNewUser :: Generic NewUser

instance arbitraryNewUser :: Arbitrary NewUser where
  arbitrary = do
    email <- arbitrary
    password <- arbitrary
    pure (NewUser {email,password})

instance eqNewUser :: Eq NewUser where
  eq = gEq

instance showNewUser :: Show NewUser where
  show = gShow

instance encodeJsonNewUser :: EncodeJson NewUser where
  encodeJson (NewUser {email,password})
    =  "email" := email
    ~> "password" := password
    ~> jsonEmptyObject

instance decodeJsonNewUser :: DecodeJson NewUser where
  decodeJson json = do
    o <- decodeJson json
    email <- o .? "email"
    password <- o .? "password"
    pure (NewUser {email,password})
