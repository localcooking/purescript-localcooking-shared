module LocalCooking.Global.Error where

import LocalCooking.Semantics.Common (RegisterError (..), ConfirmEmailError (..))
import LocalCooking.Semantics.User (UserExists (..), UserUnique (..), HasRole (..))
import LocalCooking.Semantics.Admin (SubmissionPolicyUnique (..))
import LocalCooking.Semantics.Blog
  ( BlogPostCategoryExists (..), BlogPostCategoryUnique (..)
  , BlogPostExists (..), BlogPostUnique (..), BlogPostPrimary (..))
import LocalCooking.Semantics.Chef (ChefExists (..), ChefUnique (..))
import LocalCooking.Semantics.Content
  (SubmissionPolicy (..), SubmissionExists (..), EditorExists (..))
import LocalCooking.Semantics.Mitch
  ( CustomerExists (..), CustomerUnique (..), OrderExists (..), ReviewExists (..)
  , RatingExists (..), MealExists (..), MealUnique (..), MenuExists (..), MenuUnique (..)
  , MenuPublished (..))
import LocalCooking.Semantics.Tag (TagExists (..))
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
  | AdminSubmissionPolicyNotUnique
  | BlogBlogPostCategoryDoesntExist
  | BlogBlogPostCategoryNotUnique
  | BlogBlogPostDoesntExist
  | BlogBlogPostNotUnique
  | BlogBlogPostNotPrimary
  | ChefChefDoesntExist
  | ChefChefNotUnique
  | ContentNoSubmissionPolicy
  | ContentSubmissionDoesntExist
  | ContentEditorDoesntExist
  | MitchCustomerDoesntExist
  | MitchCustomerNotUnique
  | MitchOrderDoesntExist
  | MitchReviewDoesntExist
  | MitchRatingDoesntExist
  | MitchMealDoesntExist
  | MitchMealNotUnique
  | MitchMenuDoesntExist
  | MitchMenuNotUnique
  | MitchMenuNotPublished
  | TagTagDoesntExist

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
    AdminSubmissionPolicyNotUnique -> "Error: Submission policy not unique"
    BlogBlogPostCategoryNotUnique -> "Error: Blog Post Category not unique"
    BlogBlogPostCategoryDoesntExist -> "Error: Blog Post Category doesn't exist"
    BlogBlogPostNotUnique -> "Error: Blog Post not unique"
    BlogBlogPostDoesntExist -> "Error: Blog Post doesn't exist"
    BlogBlogPostNotPrimary -> "Error: Blog Post not primary"
    ChefChefDoesntExist -> "Error: Chef doesn't exist"
    ChefChefNotUnique -> "Error: Chef not unique"
    ContentNoSubmissionPolicy -> "Error: Submission policy not identifiable"
    ContentSubmissionDoesntExist -> "Error: Submission doesn't exist"
    ContentEditorDoesntExist -> "Error: Editor doesn't exist"
    MitchCustomerDoesntExist -> "Error: Customer doesn't exist"
    MitchCustomerNotUnique -> "Error: Customer doesn't exist"
    MitchOrderDoesntExist -> "Error: Order doesn't exist"
    MitchReviewDoesntExist -> "Error: Review doesn't exist"
    MitchRatingDoesntExist -> "Error: Rating doesn't exist"
    MitchMealDoesntExist -> "Error: Meal doesn't exist"
    MitchMealNotUnique -> "Error: Meal not unique"
    MitchMenuDoesntExist -> "Error: Menu doesn't exist"
    MitchMenuNotUnique -> "Error: Menu not unique"
    MitchMenuNotPublished -> "Error: Menu not published"
    TagTagDoesntExist -> "Error: Tag doesn't exist"
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

getSubmissionPolicyUnique :: forall eff a
               . One.Queue (write :: WRITE) (ref :: REF | eff) GlobalError
              -> SubmissionPolicyUnique a
              -> (a -> Eff (ref :: REF | eff) Unit)
              -> Eff (ref :: REF | eff) Unit
getSubmissionPolicyUnique globalErrorQueue x f = case x of
  SubmissionPolicyNotUnique ->
    One.putQueue globalErrorQueue (GlobalErrorCode AdminSubmissionPolicyNotUnique)
  SubmissionPolicyUnique y -> f y

getBlogPostCategoryExists :: forall eff a
               . One.Queue (write :: WRITE) (ref :: REF | eff) GlobalError
              -> BlogPostCategoryExists a
              -> (a -> Eff (ref :: REF | eff) Unit)
              -> Eff (ref :: REF | eff) Unit
getBlogPostCategoryExists globalErrorQueue x f = case x of
  BlogPostCategoryDoesntExist ->
    One.putQueue globalErrorQueue (GlobalErrorCode BlogBlogPostCategoryDoesntExist)
  BlogPostCategoryExists y -> f y

getBlogPostCategoryUnique :: forall eff a
               . One.Queue (write :: WRITE) (ref :: REF | eff) GlobalError
              -> BlogPostCategoryUnique a
              -> (a -> Eff (ref :: REF | eff) Unit)
              -> Eff (ref :: REF | eff) Unit
getBlogPostCategoryUnique globalErrorQueue x f = case x of
  BlogPostCategoryNotUnique ->
    One.putQueue globalErrorQueue (GlobalErrorCode BlogBlogPostCategoryNotUnique)
  BlogPostCategoryUnique y -> f y

getBlogPostExists :: forall eff a
               . One.Queue (write :: WRITE) (ref :: REF | eff) GlobalError
              -> BlogPostExists a
              -> (a -> Eff (ref :: REF | eff) Unit)
              -> Eff (ref :: REF | eff) Unit
getBlogPostExists globalErrorQueue x f = case x of
  BlogPostDoesntExist ->
    One.putQueue globalErrorQueue (GlobalErrorCode BlogBlogPostDoesntExist)
  BlogPostExists y -> f y

getBlogPostUnique :: forall eff a
               . One.Queue (write :: WRITE) (ref :: REF | eff) GlobalError
              -> BlogPostUnique a
              -> (a -> Eff (ref :: REF | eff) Unit)
              -> Eff (ref :: REF | eff) Unit
getBlogPostUnique globalErrorQueue x f = case x of
  BlogPostNotUnique ->
    One.putQueue globalErrorQueue (GlobalErrorCode BlogBlogPostNotUnique)
  BlogPostUnique y -> f y

getBlogPostPrimary :: forall eff a
               . One.Queue (write :: WRITE) (ref :: REF | eff) GlobalError
              -> BlogPostPrimary a
              -> (a -> Eff (ref :: REF | eff) Unit)
              -> Eff (ref :: REF | eff) Unit
getBlogPostPrimary globalErrorQueue x f = case x of
  BlogPostNotPrimary ->
    One.putQueue globalErrorQueue (GlobalErrorCode BlogBlogPostNotPrimary)
  BlogPostPrimary y -> f y

getChefExists :: forall eff a
               . One.Queue (write :: WRITE) (ref :: REF | eff) GlobalError
              -> ChefExists a
              -> (a -> Eff (ref :: REF | eff) Unit)
              -> Eff (ref :: REF | eff) Unit
getChefExists globalErrorQueue x f = case x of
  ChefDoesntExist ->
    One.putQueue globalErrorQueue (GlobalErrorCode ChefChefDoesntExist)
  ChefExists y -> f y

getChefUnique :: forall eff a
               . One.Queue (write :: WRITE) (ref :: REF | eff) GlobalError
              -> ChefUnique a
              -> (a -> Eff (ref :: REF | eff) Unit)
              -> Eff (ref :: REF | eff) Unit
getChefUnique globalErrorQueue x f = case x of
  ChefNotUnique ->
    One.putQueue globalErrorQueue (GlobalErrorCode ChefChefNotUnique)
  ChefUnique y -> f y

getSubmissionPolicy :: forall eff a
               . One.Queue (write :: WRITE) (ref :: REF | eff) GlobalError
              -> SubmissionPolicy a
              -> (a -> Eff (ref :: REF | eff) Unit)
              -> Eff (ref :: REF | eff) Unit
getSubmissionPolicy globalErrorQueue x f = case x of
  NoSubmissionPolicy ->
    One.putQueue globalErrorQueue (GlobalErrorCode ContentNoSubmissionPolicy)
  SubmissionPolicy y -> f y

getSubmissionExists :: forall eff a
               . One.Queue (write :: WRITE) (ref :: REF | eff) GlobalError
              -> SubmissionExists a
              -> (a -> Eff (ref :: REF | eff) Unit)
              -> Eff (ref :: REF | eff) Unit
getSubmissionExists globalErrorQueue x f = case x of
  SubmissionDoesntExist ->
    One.putQueue globalErrorQueue (GlobalErrorCode ContentSubmissionDoesntExist)
  SubmissionExists y -> f y

getEditorExists :: forall eff a
               . One.Queue (write :: WRITE) (ref :: REF | eff) GlobalError
              -> EditorExists a
              -> (a -> Eff (ref :: REF | eff) Unit)
              -> Eff (ref :: REF | eff) Unit
getEditorExists globalErrorQueue x f = case x of
  EditorDoesntExist ->
    One.putQueue globalErrorQueue (GlobalErrorCode ContentEditorDoesntExist)
  EditorExists y -> f y

getCustomerExists :: forall eff a
               . One.Queue (write :: WRITE) (ref :: REF | eff) GlobalError
              -> CustomerExists a
              -> (a -> Eff (ref :: REF | eff) Unit)
              -> Eff (ref :: REF | eff) Unit
getCustomerExists globalErrorQueue x f = case x of
  CustomerDoesntExist ->
    One.putQueue globalErrorQueue (GlobalErrorCode MitchCustomerDoesntExist)
  CustomerExists y -> f y

getCustomerUnique :: forall eff a
               . One.Queue (write :: WRITE) (ref :: REF | eff) GlobalError
              -> CustomerUnique a
              -> (a -> Eff (ref :: REF | eff) Unit)
              -> Eff (ref :: REF | eff) Unit
getCustomerUnique globalErrorQueue x f = case x of
  CustomerNotUnique ->
    One.putQueue globalErrorQueue (GlobalErrorCode MitchCustomerNotUnique)
  CustomerUnique y -> f y

getOrderExists :: forall eff a
               . One.Queue (write :: WRITE) (ref :: REF | eff) GlobalError
              -> OrderExists a
              -> (a -> Eff (ref :: REF | eff) Unit)
              -> Eff (ref :: REF | eff) Unit
getOrderExists globalErrorQueue x f = case x of
  OrderDoesntExist ->
    One.putQueue globalErrorQueue (GlobalErrorCode MitchOrderDoesntExist)
  OrderExists y -> f y

getReviewExists :: forall eff a
               . One.Queue (write :: WRITE) (ref :: REF | eff) GlobalError
              -> ReviewExists a
              -> (a -> Eff (ref :: REF | eff) Unit)
              -> Eff (ref :: REF | eff) Unit
getReviewExists globalErrorQueue x f = case x of
  ReviewDoesntExist ->
    One.putQueue globalErrorQueue (GlobalErrorCode MitchReviewDoesntExist)
  ReviewExists y -> f y

getRatingExists :: forall eff a
               . One.Queue (write :: WRITE) (ref :: REF | eff) GlobalError
              -> RatingExists a
              -> (a -> Eff (ref :: REF | eff) Unit)
              -> Eff (ref :: REF | eff) Unit
getRatingExists globalErrorQueue x f = case x of
  RatingDoesntExist ->
    One.putQueue globalErrorQueue (GlobalErrorCode MitchRatingDoesntExist)
  RatingExists y -> f y

getMealExists :: forall eff a
               . One.Queue (write :: WRITE) (ref :: REF | eff) GlobalError
              -> MealExists a
              -> (a -> Eff (ref :: REF | eff) Unit)
              -> Eff (ref :: REF | eff) Unit
getMealExists globalErrorQueue x f = case x of
  MealDoesntExist ->
    One.putQueue globalErrorQueue (GlobalErrorCode MitchMealDoesntExist)
  MealExists y -> f y

getMealUnique :: forall eff a
               . One.Queue (write :: WRITE) (ref :: REF | eff) GlobalError
              -> MealUnique a
              -> (a -> Eff (ref :: REF | eff) Unit)
              -> Eff (ref :: REF | eff) Unit
getMealUnique globalErrorQueue x f = case x of
  MealNotUnique ->
    One.putQueue globalErrorQueue (GlobalErrorCode MitchMealNotUnique)
  MealUnique y -> f y

getMenuExists :: forall eff a
               . One.Queue (write :: WRITE) (ref :: REF | eff) GlobalError
              -> MenuExists a
              -> (a -> Eff (ref :: REF | eff) Unit)
              -> Eff (ref :: REF | eff) Unit
getMenuExists globalErrorQueue x f = case x of
  MenuDoesntExist ->
    One.putQueue globalErrorQueue (GlobalErrorCode MitchMenuDoesntExist)
  MenuExists y -> f y

getMenuUnique :: forall eff a
               . One.Queue (write :: WRITE) (ref :: REF | eff) GlobalError
              -> MenuUnique a
              -> (a -> Eff (ref :: REF | eff) Unit)
              -> Eff (ref :: REF | eff) Unit
getMenuUnique globalErrorQueue x f = case x of
  MenuNotUnique ->
    One.putQueue globalErrorQueue (GlobalErrorCode MitchMenuNotUnique)
  MenuUnique y -> f y

getMenuPublished :: forall eff a
               . One.Queue (write :: WRITE) (ref :: REF | eff) GlobalError
              -> MenuPublished a
              -> (a -> Eff (ref :: REF | eff) Unit)
              -> Eff (ref :: REF | eff) Unit
getMenuPublished globalErrorQueue x f = case x of
  MenuNotPublished ->
    One.putQueue globalErrorQueue (GlobalErrorCode MitchMenuNotPublished)
  MenuPublished y -> f y

getTagExists :: forall eff a
               . One.Queue (write :: WRITE) (ref :: REF | eff) GlobalError
              -> TagExists a
              -> (a -> Eff (ref :: REF | eff) Unit)
              -> Eff (ref :: REF | eff) Unit
getTagExists globalErrorQueue x f = case x of
  TagDoesntExist ->
    One.putQueue globalErrorQueue (GlobalErrorCode TagTagDoesntExist)
  TagExists y -> f y
