module LocalCooking.Global.Error where

import LocalCooking.Semantics.Common (RegisterError (..), ConfirmEmailError (..))
import LocalCooking.Semantics.User (UserExists (..), UserUnique (..), HasRole (..))
import Facebook.Types (FacebookLoginReturnError (..), FacebookUserId)

import Prelude
import Data.Maybe (Maybe (..))
import Data.NonEmpty (NonEmpty (..))
import Data.Generic (class Generic, gShow, gEq)
import Data.Argonaut (class DecodeJson, class EncodeJson, (.?), decodeJson, fail, encodeJson, (:=), (~>), jsonEmptyObject)
import Control.Alternative ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen as QC
import Queue.Types (WRITE)
import Queue.One as One



data UserEmailError
  = UserEmailNoInitOut
  | UserEmailNoAuth

derive instance genericUserEmailError :: Generic UserEmailError

instance showUserEmailError :: Show UserEmailError where
  show = gShow


data RedirectError
  = RedirectRegisterAuth
  | RedirectUserDetailsNoAuth
  | RedirectLogout

derive instance genericRedirectError :: Generic RedirectError

instance showRedirectError :: Show RedirectError where
  show = gShow


-- data CustomerError
--   = CustomerSaveFailed
--   | CustomerSaveSuccess

-- derive instance genericCustomerError :: Generic CustomerError

-- instance showCustomerError :: Show CustomerError where
--   show = gShow


data SecurityMessage
  = SecuritySaveFailed
  | SecuritySaveSuccess

derive instance genericSecurityMessage :: Generic SecurityMessage

instance showSecurityMessage :: Show SecurityMessage where
  show = gShow


data AuthTokenFailure
  = FBLoginReturnBad String String
  | FBLoginReturnDenied String
  | FBLoginReturnBadParse
  | FBLoginReturnNoUser FacebookUserId
  | FBLoginReturnError FacebookLoginReturnError
  | AuthLoginFailure
  | AuthTokenExpired

derive instance genericAuthTokenFailure :: Generic AuthTokenFailure

instance arbitraryAuthTokenFailure :: Arbitrary AuthTokenFailure where
  arbitrary = QC.oneOf $ NonEmpty
    ( pure AuthLoginFailure
    )
    [ pure AuthTokenExpired
    , FBLoginReturnBad <$> arbitrary <*> arbitrary
    , FBLoginReturnDenied <$> arbitrary
    , pure FBLoginReturnBadParse
    , FBLoginReturnNoUser <$> arbitrary
    , FBLoginReturnError <$> arbitrary
    ]

instance showAuthTokenFailure :: Show AuthTokenFailure where
  show = gShow

instance eqAuthTokenFailure :: Eq AuthTokenFailure where
  eq = gEq

instance encodeJsonAuthTokenFailure :: EncodeJson AuthTokenFailure where
  encodeJson x = case x of
    AuthLoginFailure -> encodeJson "loginFailure"
    AuthTokenExpired -> encodeJson "tokenExpired"
    FBLoginReturnBadParse -> encodeJson "bad-parse"
    FBLoginReturnBad code msg
      -> "fbBad" :=
         ( "code" := code
         ~> "msg" := msg
         ~> jsonEmptyObject
         )
      ~> jsonEmptyObject
    FBLoginReturnDenied desc
      -> "fbDenied" :=
         ( "desc" := desc
         ~> jsonEmptyObject
         )
      ~> jsonEmptyObject
    FBLoginReturnNoUser y
      -> "no-user" := y
      ~> jsonEmptyObject
    FBLoginReturnError y
      -> "fbLoginReturnError" := y
      ~> jsonEmptyObject

instance decodeJsonAuthTokenFailure :: DecodeJson AuthTokenFailure where
  decodeJson json = do
    let obj = do
          o <- decodeJson json
          let fbBad = do
                o' <- o .? "fbBad"
                code <- o' .? "code"
                msg <- o' .? "msg"
                pure $ FBLoginReturnBad code msg
              fbDenied = do
                o' <- o .? "fbDenied"
                desc <- o' .? "desc"
                pure $ FBLoginReturnDenied desc
              fbNoUser = do
                x <- o .? "no-user"
                pure $ FBLoginReturnNoUser x
              fbReturn = do
                x <- o .? "fbLoginReturnError"
                pure $ FBLoginReturnError x
          fbBad <|> fbDenied <|> fbNoUser <|> fbReturn
        str = do
          s <- decodeJson json
          case unit of
            _ | s == "bad-parse" -> pure FBLoginReturnBadParse
              | s == "loginFailure" -> pure AuthLoginFailure
              | s == "tokenExpired" -> pure AuthTokenExpired
              | otherwise -> fail "Not a AuthTokenFailure"
    obj <|> str



-- | From JSON-compatible error types
data ErrorCode
  = UserUserDoesntExist
  | UserDoesntHaveRole
  | UserUserNotUnique

derive instance genericErrorCode :: Generic ErrorCode

instance showErrorCode :: Show ErrorCode where
  show = gShow


data GlobalError
  = GlobalErrorAuthFailure AuthTokenFailure
  | GlobalErrorUserEmail UserEmailError
  | GlobalErrorRegister (Maybe RegisterError)
  | GlobalErrorRedirect RedirectError
  | GlobalErrorSecurity SecurityMessage
  | GlobalErrorConfirmEmail ConfirmEmailError
  | GlobalErrorCode ErrorCode
  -- | GlobalErrorCustomer CustomerError

derive instance genericGlobalError :: Generic GlobalError

instance showGlobalError :: Show GlobalError where
  show = gShow



printGlobalError :: GlobalError -> String
printGlobalError x = case x of
  GlobalErrorCode errCode -> case errCode of
    UserUserDoesntExist -> "Error: User doesn't exist"
    UserDoesntHaveRole -> "Error: User doesn't have role"
    UserUserNotUnique -> "Error: User not unique"
  GlobalErrorAuthFailure authFailure -> case authFailure of
    FBLoginReturnBad a b -> "Facebook login failed: " <> a <> ", " <> b
    FBLoginReturnDenied a -> "Facebook login denied: " <> a
    FBLoginReturnBadParse -> "Internal error - bad Facebook login"
    FBLoginReturnNoUser _ -> "Facebook user not recognized"
    FBLoginReturnError fb -> case fb of
      FacebookLoginVerifyParseFailure a -> "Facebook parse failure: " <> a
      FacebookLoginUserDetailsParseFailure a -> "Facebook parse failure: " <> a
      FacebookLoginGetTokenError' a b c d -> "Facebook get token error: " <> a <> ", " <> b <> ", " <> show c <> ", " <> d
    AuthLoginFailure -> "Password incorrect, please try again."
    AuthTokenExpired -> "Session expired, please login."
  GlobalErrorUserEmail userEmail -> case userEmail of
    UserEmailNoInitOut -> "Internal Error: userEmail resource failed"
    UserEmailNoAuth -> "Error: No authorization for email"
  GlobalErrorRegister mRegister -> case mRegister of
    Nothing -> "Registered! Please check your spam folder and confirm in 7 days."
    Just register -> case register of
      RegisterDecodingError e -> "Couldn't decode register somehow: " <> e
      RegisterReCaptchaFailure e -> "ReCaptcha response failure: " <> e
      RegisterEmailTaken -> "Email address is already registered."
  GlobalErrorRedirect redirect -> case redirect of
    RedirectRegisterAuth -> "Redirected - can't register while logged in."
    RedirectUserDetailsNoAuth -> "Redirected - can't view user details while logged out."
    RedirectLogout -> "Redirected - you've logged out."
  GlobalErrorSecurity sec -> case sec of
    SecuritySaveFailed -> "Error - Securty save failed."
    SecuritySaveSuccess -> "Security details saved."
  GlobalErrorConfirmEmail email -> case email of
    ConfirmEmailTokenNonexistent -> "Error - email token nonexistent"
    ConfirmEmailUserNonexistent -> "Error - user nonexistent"
    ConfirmEmailOk -> "Email confirmed!"
  -- GlobalErrorCustomer cust -> case cust of
  --   CustomerSaveFailed -> "Internal error - couldn't save customer details"
  --   CustomerSaveSuccess -> "Customer details saved"


-- * Error code processing

getUserExists :: forall eff a
               . One.Queue (write :: WRITE) (ref :: REF | eff) GlobalError
              -> UserExists a
              -> (a -> Eff (ref :: REF | eff) Unit)
              -> Eff (ref :: REF | eff) Unit
getUserExists globalErrorQueue x f = case x of
  UserDoesntExist ->
    One.putQueue globalErrorQueue (GlobalErrorCode UserUserDoesntExist)
  UserExists y -> f y

getUserUnique :: forall eff a
               . One.Queue (write :: WRITE) (ref :: REF | eff) GlobalError
              -> UserUnique a
              -> (a -> Eff (ref :: REF | eff) Unit)
              -> Eff (ref :: REF | eff) Unit
getUserUnique globalErrorQueue x f = case x of
  UserNotUnique ->
    One.putQueue globalErrorQueue (GlobalErrorCode UserUserNotUnique)
  UserUnique y -> f y

getHasRole :: forall eff a
               . One.Queue (write :: WRITE) (ref :: REF | eff) GlobalError
              -> HasRole a
              -> (a -> Eff (ref :: REF | eff) Unit)
              -> Eff (ref :: REF | eff) Unit
getHasRole globalErrorQueue x f = case x of
  DoesntHaveRole ->
    One.putQueue globalErrorQueue (GlobalErrorCode UserDoesntHaveRole)
  HasRole y -> f y
