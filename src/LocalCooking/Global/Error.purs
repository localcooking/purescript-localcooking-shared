module LocalCooking.Global.Error where

import LocalCooking.Semantics.Common (RegisterError (..), ConfirmEmailError (..))
import Facebook.Types (FacebookLoginReturnError (..), FacebookUserId)

import Prelude
import Data.Maybe (Maybe (..))
import Data.NonEmpty (NonEmpty (..))
import Data.Generic (class Generic, gShow, gEq)
import Data.Argonaut (class DecodeJson, class EncodeJson, (.?), decodeJson, fail, encodeJson, (:=), (~>), jsonEmptyObject)
import Control.Alternative ((<|>))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen as QC



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
  | AuthTokenLoginFailure

derive instance genericAuthTokenFailure :: Generic AuthTokenFailure

instance arbitraryAuthTokenFailure :: Arbitrary AuthTokenFailure where
  arbitrary = QC.oneOf $ NonEmpty
    ( pure AuthTokenLoginFailure
    )
    [ FBLoginReturnBad <$> arbitrary <*> arbitrary
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
    AuthTokenLoginFailure -> encodeJson "loginFailure"
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
              | s == "loginFailure" -> pure AuthTokenLoginFailure
              | otherwise -> fail "Not a AuthTokenFailure"
    obj <|> str



data GlobalError
  = GlobalErrorAuthFailure AuthTokenFailure
  | GlobalErrorUserEmail UserEmailError
  | GlobalErrorRegister (Maybe RegisterError)
  | GlobalErrorRedirect RedirectError
  | GlobalErrorSecurity SecurityMessage
  | GlobalErrorConfirmEmail ConfirmEmailError

derive instance genericGlobalError :: Generic GlobalError

instance showGlobalError :: Show GlobalError where
  show = gShow



printGlobalError :: GlobalError -> String
printGlobalError x = case x of
  GlobalErrorAuthFailure authFailure -> case authFailure of
    FBLoginReturnBad a b -> "Facebook login failed: " <> a <> ", " <> b
    FBLoginReturnDenied a -> "Facebook login denied: " <> a
    FBLoginReturnBadParse -> "Internal error - bad Facebook login"
    FBLoginReturnNoUser _ -> "Facebook user not recognized"
    FBLoginReturnError fb -> case fb of
      FacebookLoginVerifyParseFailure a -> "Facebook parse failure: " <> a
      FacebookLoginUserDetailsParseFailure a -> "Facebook parse failure: " <> a
      FacebookLoginGetTokenError' a b c d -> "Facebook get token error: " <> a <> ", " <> b <> ", " <> show c <> ", " <> d
    AuthTokenLoginFailure -> "Password incorrect, please try again."
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
