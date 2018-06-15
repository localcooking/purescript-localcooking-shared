module LocalCooking.Semantics.Admin where

import LocalCooking.Semantics.Common (User)
import LocalCooking.Common.User.Password (HashedPassword)

import Prelude
import Data.Maybe (Maybe)
import Data.NonEmpty (NonEmpty (..))
import Data.Generic (class Generic, gEq, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, (:=), (~>), jsonEmptyObject, (.?))
import Control.Alternative ((<|>))
import Text.Email.Validate (EmailAddress)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)



data SetUser
  = SetUserUpdate
    { user :: User
    , newPassword :: Maybe HashedPassword
    }
  | SetUserDelete User

derive instance genericSetUser :: Generic SetUser

instance arbitrarySetUser :: Arbitrary SetUser where
  arbitrary = oneOf $ NonEmpty
    ( do
      user <- arbitrary
      newPassword <- arbitrary
      pure (SetUserUpdate {user,newPassword})
    )
    [ SetUserDelete <$> arbitrary
    ]

instance eqSetUser :: Eq SetUser where
  eq = gEq

instance showSetUser :: Show SetUser where
  show = gShow

instance encodeJsonSetUser :: EncodeJson SetUser where
  encodeJson x = case x of
    SetUserUpdate {user,newPassword}
      -> "setUserUpdate"
      := ( "user" := user
        ~> "newPassword" := newPassword
        ~> jsonEmptyObject
        )
      ~> jsonEmptyObject
    SetUserDelete user
      -> "setUserDelete"
      := ( "user" := user
        ~> jsonEmptyObject
         )
      ~> jsonEmptyObject

instance decodeJsonSetUser :: DecodeJson SetUser where
  decodeJson json = do
    o <- decodeJson json
    let update = do
          o' <- o .? "setUserUpdate"
          user <- o' .? "user"
          newPassword <- o' .? "newPassword"
          pure (SetUserUpdate {user,newPassword})
        delete = do
          o' <- o .? "setUserDelete"
          user <- o' .? "user"
          pure (SetUserDelete user)
    update <|> delete


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
