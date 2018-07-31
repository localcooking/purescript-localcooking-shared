module LocalCooking.Semantics.Admin where

import LocalCooking.Semantics.Common (User)
import LocalCooking.Semantics.ContentRecord.Variant (ContentRecordVariant)
import LocalCooking.Database.Schema (StoredEditorId)

import Prelude
import Data.Password (HashedPassword)
import Data.Maybe (Maybe)
import Data.NonEmpty (NonEmpty (..))
import Data.Generic (class Generic, gEq, gShow)
import Data.Argonaut
  ( class EncodeJson, class DecodeJson, encodeJson, decodeJson
  , fail, (:=), (~>), jsonEmptyObject, (.?))
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



newtype GetSetSubmissionPolicy = GetSetSubmissionPolicy
  { variant :: ContentRecordVariant
  , additional :: Int
  , assigned :: Array StoredEditorId
  }

derive instance genericGetSetSubmissionPolicy :: Generic GetSetSubmissionPolicy

instance arbitraryGetSetSubmissionPolicy :: Arbitrary GetSetSubmissionPolicy where
  arbitrary = do
    variant <- arbitrary
    additional <- arbitrary
    assigned <- arbitrary
    pure (GetSetSubmissionPolicy {variant,additional,assigned})

instance eqGetSetSubmissionPolicy :: Eq GetSetSubmissionPolicy where
  eq = gEq

instance showGetSetSubmissionPolicy :: Show GetSetSubmissionPolicy where
  show = gShow

instance encodeJsonGetSetSubmissionPolicy :: EncodeJson GetSetSubmissionPolicy where
  encodeJson (GetSetSubmissionPolicy {variant,additional,assigned})
    =  "variant" := variant
    ~> "additional" := additional
    ~> "assigned" := assigned
    ~> jsonEmptyObject

instance decodeJsonGetSetSubmissionPolicy :: DecodeJson GetSetSubmissionPolicy where
  decodeJson json = do
    o <- decodeJson json
    variant <- o .? "variant"
    additional <- o .? "additional"
    assigned <- o .? "assigned"
    pure (GetSetSubmissionPolicy {variant,additional,assigned})


-- * Errors



data SubmissionPolicyUnique a
  = SubmissionPolicyNotUnique
  | SubmissionPolicyUnique a

derive instance genericSubmissionPolicyUnique :: Generic a => Generic (SubmissionPolicyUnique a)

instance arbitrarySubmissionPolicyUnique :: Arbitrary a => Arbitrary (SubmissionPolicyUnique a) where
  arbitrary = oneOf $ NonEmpty
    ( pure SubmissionPolicyNotUnique
    )
    [ SubmissionPolicyUnique <$> arbitrary
    ]

instance eqSubmissionPolicyUnique :: Generic a => Eq (SubmissionPolicyUnique a) where
  eq = gEq

instance showSubmissionPolicyUnique :: Generic a => Show (SubmissionPolicyUnique a) where
  show = gShow

instance encodeJsonSubmissionPolicyUnique :: EncodeJson a => EncodeJson (SubmissionPolicyUnique a) where
  encodeJson x = case x of
    SubmissionPolicyNotUnique -> encodeJson "submissionPolicyNotUnique"
    SubmissionPolicyUnique y
      -> "submissionPolicyUnique"
      := y
      ~> jsonEmptyObject

instance decodeJsonSubmissionPolicyUnique :: DecodeJson a => DecodeJson (SubmissionPolicyUnique a) where
  decodeJson json = do
    let empty = do
          s <- decodeJson json
          if s == "submissionPolicyNotUnique"
             then pure SubmissionPolicyNotUnique
             else fail "Not a SubmissionPolicyUnique"
        has = do
          o <- decodeJson json
          SubmissionPolicyUnique <$> o .? "submissionPolicyUnique"
    empty <|> has
