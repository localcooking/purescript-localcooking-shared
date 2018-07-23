module LocalCooking.Semantics.Common where

import LocalCooking.Database.Schema (StoredUserId)
import LocalCooking.Common.User.Role (UserRole)
import LocalCooking.Common.User.Password (HashedPassword)
import Facebook.Types (FacebookUserId, FacebookLoginCode)
import Google.ReCaptcha (ReCaptchaResponse)

import Prelude
import Data.DateTime (DateTime)
import Data.DateTime.JSON (JSONDateTime (..))
import Data.Maybe (Maybe)
import Data.NonEmpty (NonEmpty (..))
import Data.Generic (class Generic, gEq, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, (:=), (~>), jsonEmptyObject, (.?), fail)
import Control.Alternative ((<|>))
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
  , created        :: DateTime
  , email          :: EmailAddress
  , socialLogin    :: SocialLoginForm
  , emailConfirmed :: Boolean
  , roles          :: Array UserRole
  }

derive instance genericUser :: Generic User

instance arbitraryUser :: Arbitrary User where
  arbitrary = do
    id <- arbitrary
    JSONDateTime created <- arbitrary
    email <- arbitrary
    socialLogin <- arbitrary
    emailConfirmed <- arbitrary
    roles <- arbitrary
    pure (User {id,created,email,socialLogin,emailConfirmed,roles})

instance eqUser :: Eq User where
  eq = gEq

instance showUser :: Show User where
  show = gShow

instance encodeJsonUser :: EncodeJson User where
  encodeJson (User {id,created,email,socialLogin,emailConfirmed,roles})
    =  "id" := id
    ~> "created" := JSONDateTime created
    ~> "email" := email
    ~> "socialLogin" := socialLogin
    ~> "emailConfirmed" := emailConfirmed
    ~> "roles" := roles
    ~> jsonEmptyObject

instance decodeJsonUser :: DecodeJson User where
  decodeJson json = do
    o <- decodeJson json
    id <- o .? "id"
    JSONDateTime created <- o .? "created"
    email <- o .? "email"
    socialLogin <- o .? "socialLogin"
    emailConfirmed <- o .? "emailConfirmed"
    roles <- o .? "roles"
    pure (User {id,created,email,socialLogin,emailConfirmed,roles})


newtype ChangePassword = ChangePassword
  { oldPassword :: HashedPassword
  , newPassword :: HashedPassword
  }

derive instance genericChangePassword :: Generic ChangePassword

instance arbitraryChangePassword :: Arbitrary ChangePassword where
  arbitrary = do
    oldPassword <- arbitrary
    newPassword <- arbitrary
    pure (ChangePassword {oldPassword,newPassword})

instance eqChangePassword :: Eq ChangePassword where
  eq = gEq

instance showChangePassword :: Show ChangePassword where
  show = gShow

instance encodeJsonChangePassword :: EncodeJson ChangePassword where
  encodeJson (ChangePassword {oldPassword,newPassword})
    =  "oldPassword" := oldPassword
    ~> "newPassword" := newPassword
    ~> jsonEmptyObject

instance decodeJsonChangePassword :: DecodeJson ChangePassword where
  decodeJson json = do
    o <- decodeJson json
    oldPassword <- o .? "oldPassword"
    newPassword <- o .? "newPassword"
    pure (ChangePassword {oldPassword,newPassword})


newtype SetUser = SetUser
  { id :: StoredUserId
  , email :: Maybe EmailAddress
  , socialLogin :: Maybe SocialLoginForm
  , changePassword :: Maybe ChangePassword
  }

derive instance genericSetUser :: Generic SetUser

instance arbitrarySetUser :: Arbitrary SetUser where
  arbitrary = do
    id <- arbitrary
    email <- arbitrary
    socialLogin <- arbitrary
    changePassword <- arbitrary
    pure (SetUser {id,email,socialLogin,changePassword})

instance eqSetUser :: Eq SetUser where
  eq = gEq

instance showSetUser :: Show SetUser where
  show = gShow

instance encodeJsonSetUser :: EncodeJson SetUser where
  encodeJson (SetUser {id,email,socialLogin,changePassword})
    =  "id" := id
    ~> "email" := email
    ~> "socialLogin" := socialLogin
    ~> "changePassword" := changePassword
    ~> jsonEmptyObject

instance decodeJsonSetUser :: DecodeJson SetUser where
  decodeJson json = do
    o <- decodeJson json
    id <- o .? "id"
    email <- o .? "email"
    socialLogin <- o .? "socialLogin"
    changePassword <- o .? "changePassword"
    pure (SetUser {id,email,socialLogin,changePassword})




-- TODO FIXME phone register / login
newtype Register = Register
  { email     :: EmailAddress
  , password  :: HashedPassword
  , social    :: SocialLoginForm
  , reCaptcha :: ReCaptchaResponse
  }

derive instance genericRegister :: Generic Register

instance arbitraryRegister :: Arbitrary Register where
  arbitrary = do
    email <- arbitrary
    password <- arbitrary
    social <- arbitrary
    reCaptcha <- arbitrary
    pure (Register {email,password,social,reCaptcha})

instance eqRegister :: Eq Register where
  eq = gEq

instance showRegister :: Show Register where
  show = gShow

instance encodeJsonRegister :: EncodeJson Register where
  encodeJson (Register {email,password,social,reCaptcha})
    =  "email" := email
    ~> "password" := password
    ~> "social" := social
    ~> "reCaptcha" := reCaptcha
    ~> jsonEmptyObject

instance decodeJsonRegister :: DecodeJson Register where
  decodeJson json = do
    o <- decodeJson json
    email <- o .? "email"
    password <- o .? "password"
    social <- o .? "social"
    reCaptcha <- o .? "reCaptcha"
    pure (Register {email,password,social,reCaptcha})


data RegisterError
  = RegisterDecodingError String
  | RegisterReCaptchaFailure String
  | RegisterEmailTaken

derive instance genericRegisterError :: Generic RegisterError

instance showRegisterError :: Show RegisterError where
  show = gShow

instance eqRegisterError :: Eq RegisterError where
  eq = gEq

instance arbitraryRegisterError :: Arbitrary RegisterError where
  arbitrary = oneOf $ NonEmpty
    ( pure RegisterEmailTaken
    )
    [ RegisterDecodingError <$> arbitrary
    , RegisterReCaptchaFailure <$> arbitrary
    ]

instance encodeJsonRegisterError :: EncodeJson RegisterError where
  encodeJson x = case x of
    RegisterDecodingError e
      -> "decodingError" := e ~> jsonEmptyObject
    RegisterReCaptchaFailure e
      -> "reCaptchaFailure" := e ~> jsonEmptyObject
    RegisterEmailTaken -> encodeJson "emailTaken"

instance decodeJsonRegisterError :: DecodeJson RegisterError where
  decodeJson json = do
    let obj = do
          o <- decodeJson json
          let decodingError = RegisterDecodingError <$> o .? "decodingError"
              reCaptchaFailure = RegisterReCaptchaFailure <$> o .? "reCaptchaFailure"
          decodingError <|> reCaptchaFailure
        str = do
          s <- decodeJson json
          case unit of
            _ | s == "emailTaken" -> pure RegisterEmailTaken
              | otherwise -> fail "RegisterError"
    obj <|> str


data ConfirmEmailError
  = ConfirmEmailTokenNonexistent
  | ConfirmEmailUserNonexistent
  | ConfirmEmailOk

derive instance genericConfirmEmailError :: Generic ConfirmEmailError

instance showConfirmEmailError :: Show ConfirmEmailError where
  show = gShow

instance eqConfirmEmailError :: Eq ConfirmEmailError where
  eq = gEq

instance arbitraryConfirmEmailError :: Arbitrary ConfirmEmailError where
  arbitrary = oneOf $ NonEmpty
    ( pure ConfirmEmailTokenNonexistent
    )
    [ pure ConfirmEmailUserNonexistent
    , pure ConfirmEmailOk
    ]

instance encodeJsonConfirmEmailError :: EncodeJson ConfirmEmailError where
  encodeJson x = case x of
    ConfirmEmailTokenNonexistent -> encodeJson "tokenNonexistent"
    ConfirmEmailUserNonexistent -> encodeJson "userNonexistent"
    ConfirmEmailOk -> encodeJson "ok"

instance decodeJsonConfirmEmailError :: DecodeJson ConfirmEmailError where
  decodeJson json = do
    s <- decodeJson json
    case unit of
      _ | s == "tokenNonexistent" -> pure ConfirmEmailTokenNonexistent
        | s == "userNonexistent" -> pure ConfirmEmailUserNonexistent
        | s == "ok" -> pure ConfirmEmailOk
        | otherwise -> fail "ConfirmEmailError"


newtype Login = Login
  { email     :: EmailAddress
  , password  :: HashedPassword
  }

derive instance genericLogin :: Generic Login

instance arbitraryLogin :: Arbitrary Login where
  arbitrary = do
    email <- arbitrary
    password <- arbitrary
    pure (Login {email,password})

instance eqLogin :: Eq Login where
  eq = gEq

instance showLogin :: Show Login where
  show = gShow

instance encodeJsonLogin :: EncodeJson Login where
  encodeJson (Login {email,password})
    =  "email" := email
    ~> "password" := password
    ~> jsonEmptyObject

instance decodeJsonLogin :: DecodeJson Login where
  decodeJson json = do
    o <- decodeJson json
    email <- o .? "email"
    password <- o .? "password"
    pure (Login {email,password})




data SocialLogin
  = SocialLoginFB
    { fbCode :: FacebookLoginCode
    }

derive instance genericSocialLogin :: Generic SocialLogin

instance arbitrarySocialLogin :: Arbitrary SocialLogin where
  arbitrary = do
    fbCode <- arbitrary
    pure (SocialLoginFB {fbCode})

instance eqSocialLogin :: Eq SocialLogin where
  eq = gEq

instance showSocialLogin :: Show SocialLogin where
  show = gShow

instance encodeJsonSocialLogin :: EncodeJson SocialLogin where
  encodeJson x = case x of
    SocialLoginFB {fbCode}
      -> "fbCode" := fbCode
      ~> jsonEmptyObject

instance decodeJsonSocialLogin :: DecodeJson SocialLogin where
  decodeJson json = do
    o <- decodeJson json
    let socialFB = do
          fbCode <- o .? "fbCode"
          pure (SocialLoginFB {fbCode})
    socialFB



newtype WithId k a = WithId
  { id :: k
  , content :: a
  }

derive instance genericWithId :: (Generic k, Generic a) => Generic (WithId k a)

instance showWithId :: (Generic k, Generic a) => Show (WithId k a) where
  show = gShow

instance eqWithId :: (Generic k, Generic a) => Eq (WithId k a) where
  eq = gEq

instance arbitraryWithId :: (Arbitrary k, Arbitrary a) => Arbitrary (WithId k a) where
  arbitrary = do
    id <- arbitrary
    content <- arbitrary
    pure (WithId {id,content})

instance encodeJsonWithId :: (EncodeJson k, EncodeJson a) => EncodeJson (WithId k a) where
  encodeJson (WithId {id,content})
    =  "id" := id
    ~> "content" := content
    ~> jsonEmptyObject

instance decodeJsonWithId :: (DecodeJson k, DecodeJson a) => DecodeJson (WithId k a) where
  decodeJson json = do
    o <- decodeJson json
    id <- o .? "id"
    content <- o .? "content"
    pure (WithId {id,content})
