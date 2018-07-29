module LocalCooking.Semantics.Validate where

import LocalCooking.Common.User.Password (HashedPassword)

import Prelude
import Data.Date (Date)
import Data.Date.JSON (JSONDate (..))
import Data.String.Permalink (Permalink)
import Data.Generic (class Generic, gEq, gShow)
import Text.Email.Validate (EmailAddress)
import Data.Argonaut
  ( class EncodeJson, class DecodeJson, decodeJson
  , (:=), (~>), jsonEmptyObject, (.?))
import Test.QuickCheck (class Arbitrary, arbitrary)



newtype IsUniqueMenuDeadline = IsUniqueMenuDeadline
  { chef :: Permalink
  , deadline :: Date
  }

derive instance genericIsUniqueMenuDeadline :: Generic IsUniqueMenuDeadline

instance eqIsUniqueMenuDeadline :: Eq IsUniqueMenuDeadline where
  eq = gEq

instance showIsUniqueMenuDeadline :: Show IsUniqueMenuDeadline where
  show = gShow

instance arbitraryIsUniqueMenuDeadline :: Arbitrary IsUniqueMenuDeadline where
  arbitrary = do
    chef <- arbitrary
    JSONDate deadline <- arbitrary
    pure (IsUniqueMenuDeadline {chef,deadline})

instance encodeJsonIsUniqueMenuDeadline :: EncodeJson IsUniqueMenuDeadline where
  encodeJson (IsUniqueMenuDeadline {chef,deadline})
    =  "chef" := chef
    ~> "deadline" := JSONDate deadline
    ~> jsonEmptyObject

instance decodeJsonIsUniqueMenuDeadline :: DecodeJson IsUniqueMenuDeadline where
  decodeJson json = do
    o <- decodeJson json
    chef <- o .? "chef"
    JSONDate deadline <- o .? "deadline"
    pure (IsUniqueMenuDeadline {chef,deadline})


newtype IsUniqueMealPermalink = IsUniqueMealPermalink
  { chef :: Permalink
  , deadline :: Date
  , meal :: Permalink
  }

derive instance genericIsUniqueMealPermalink :: Generic IsUniqueMealPermalink

instance eqIsUniqueMealPermalink :: Eq IsUniqueMealPermalink where
  eq = gEq

instance showIsUniqueMealPermalink :: Show IsUniqueMealPermalink where
  show = gShow

instance arbitraryIsUniqueMealPermalink :: Arbitrary IsUniqueMealPermalink where
  arbitrary = do
    chef <- arbitrary
    JSONDate deadline <- arbitrary
    meal <- arbitrary
    pure (IsUniqueMealPermalink {chef,deadline,meal})

instance encodeJsonIsUniqueMealPermalink :: EncodeJson IsUniqueMealPermalink where
  encodeJson (IsUniqueMealPermalink {chef,deadline,meal})
    =  "chef" := chef
    ~> "deadline" := JSONDate deadline
    ~> "meal" := meal
    ~> jsonEmptyObject

instance decodeJsonIsUniqueMealPermalink :: DecodeJson IsUniqueMealPermalink where
  decodeJson json = do
    o <- decodeJson json
    chef <- o .? "chef"
    JSONDate deadline <- o .? "deadline"
    meal <- o .? "meal"
    pure (IsUniqueMealPermalink {chef,deadline,meal})


newtype PasswordVerifyUnauth = PasswordVerifyUnauth
  { email :: EmailAddress
  , password :: HashedPassword
  }

derive instance genericPasswordVerifyUnauth :: Generic PasswordVerifyUnauth

instance eqPasswordVerifyUnauth :: Eq PasswordVerifyUnauth where
  eq = gEq

instance showPasswordVerifyUnauth :: Show PasswordVerifyUnauth where
  show = gShow

instance arbitraryPasswordVerifyUnauth :: Arbitrary PasswordVerifyUnauth where
  arbitrary = do
    email <- arbitrary
    password <- arbitrary
    pure (PasswordVerifyUnauth {email,password})

instance encodeJsonPasswordVerifyUnauth :: EncodeJson PasswordVerifyUnauth where
  encodeJson (PasswordVerifyUnauth {email,password})
    =  "email" := email
    ~> "password" := password
    ~> jsonEmptyObject

instance decodeJsonPasswordVerifyUnauth :: DecodeJson PasswordVerifyUnauth where
  decodeJson json = do
    o <- decodeJson json
    email <- o .? "email"
    password <- o .? "password"
    pure (PasswordVerifyUnauth {email,password})

