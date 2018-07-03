module Test.Main where

import Data.Address (USAAddress)
import Data.Image.Source (ImageSource)
import Data.List.Pagination (PaginationArgs)
import Data.List.Sorting (SortingArgs)
import Data.Price (Price)
import Data.String.Markdown (MarkdownText)
import Data.String.Permalink (Permalink, permalinkParser)
import Data.Date.JSON (JSONDate)
import Data.DateTime.JSON (JSONDateTime)

import LocalCooking.Common.AccessToken (AccessToken)
import LocalCooking.Common.Ingredient (Ingredient)
import LocalCooking.Common.Order (OrderProgress)
import LocalCooking.Common.Rating (Rating)
import LocalCooking.Common.Tag (Tag)
import LocalCooking.Common.User.Name (Name)
import LocalCooking.Common.User.Password (HashedPassword)
import LocalCooking.Common.User.Role (UserRole)

import LocalCooking.Semantics.Common (SocialLoginForm, User, SetUser, Register, RegisterError, Login, SocialLogin)
import LocalCooking.Semantics.Mitch as Mitch
import LocalCooking.Semantics.Chef as Chef
import LocalCooking.Semantics.Admin as Admin

import LocalCooking.Global.Error (AuthTokenFailure)
import LocalCooking.Global.Links.Class (class LocalCookingSiteLinks, class LocalCookingUserDetailsLinks, defaultSiteLinksPathParser)

import Facebook.State (FacebookLoginUnsavedFormData, FacebookLoginState)

import Prelude
import Data.Either (Either (..))
import Data.Maybe (Maybe (..))
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Data.Generic (class Generic, gEq, gShow)
import Data.URI.Path as URIPath
import Data.URI.Location (Location (..), class FromLocation, class ToLocation, fromLocation, toLocation, printLocation)
import Data.Path.Pathy ((</>), dir, file, rootDir, Path, Rel, File, Sandboxed)
import Data.NonEmpty (NonEmpty (..), (:|))
import Text.Parsing.StringParser (Parser, runParser, try)
import Text.Parsing.StringParser.String (char, string)
import Type.Proxy (Proxy (..))
import Control.Alternative ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.QuickCheck (class Arbitrary, arbitrary, quickCheck, Result (..))
import Test.QuickCheck.Gen (oneOf)


data TestUserDetails
  = UserDetailsGeneralLink
  | UserDetailsSecurityLink

derive instance genericTestUserDetails :: Generic TestUserDetails

instance eqTestUserDetails :: Eq TestUserDetails where
  eq = gEq

instance showTestUserDetails :: Show TestUserDetails where
  show = gShow

instance arbitraryTestUserDetails :: Arbitrary TestUserDetails where
  arbitrary = oneOf $
    ( pure UserDetailsGeneralLink
    ) :|
    [ pure UserDetailsSecurityLink
    ]

instance localCookingTestUserDetailsTestUserDetails :: LocalCookingUserDetailsLinks TestUserDetails where
  userDetailsGeneralLink = UserDetailsGeneralLink
  userDetailsSecurityLink = UserDetailsSecurityLink
  toUserDetailsDocumentTitle x = case x of
    UserDetailsGeneralLink   -> "General - "
    UserDetailsSecurityLink  -> "Security - "

userDetailsLinksToPath :: TestUserDetails -> Path Rel File Sandboxed
userDetailsLinksToPath x = case x of
  UserDetailsGeneralLink -> file "general"
  UserDetailsSecurityLink -> file "security"

userDetailsLinksParser :: Parser TestUserDetails
userDetailsLinksParser = do
  void divider
  let general = do
        void (string "general")
        pure UserDetailsGeneralLink
      security = do
        void (string "security")
        pure UserDetailsSecurityLink
  try general
    <|> security
  where
    divider = char '/'


data TestSiteLinks
  = RegisterLink
  | RootLink (Maybe Permalink)
  | NewBlogPostLink
  | EmailConfirmLink
  | UserDetailsLink (Maybe TestUserDetails)

instance arbitraryTestSiteLinks :: Arbitrary TestSiteLinks where
  arbitrary = oneOf $
        (RootLink <$> arbitrary)
    :|  [ pure RegisterLink
        , pure NewBlogPostLink
        , UserDetailsLink <$> arbitrary
        , pure EmailConfirmLink
        ]

derive instance genericTestSiteLinks :: Generic TestSiteLinks

instance showTestSiteLinks :: Show TestSiteLinks where
  show = gShow -- printLocation <<< toLocation

instance eqTestSiteLinks :: Eq TestSiteLinks where
  eq = gEq


instance toLocationTestSiteLinks :: ToLocation TestSiteLinks where
  toLocation x = case x of
    RootLink mPost -> case mPost of
      Nothing -> Location (Left rootDir) Nothing Nothing
      Just post -> Location (Right $ rootDir </> file (show post)) Nothing Nothing
    NewBlogPostLink -> Location (Right $ rootDir </> file "newBlogPost") Nothing Nothing
    RegisterLink -> Location (Right $ rootDir </> file "register") Nothing Nothing
    EmailConfirmLink -> Location (Right $ rootDir </> file "emailConfirm") Nothing Nothing
    UserDetailsLink mUserDetails ->
      Location
        ( Right $ case mUserDetails of
             Nothing -> rootDir </> file "userDetails"
             Just d -> rootDir </> dir "userDetails" </> userDetailsLinksToPath d
        ) Nothing Nothing


instance localCookingTestSiteLinksTestSiteLinks :: LocalCookingSiteLinks TestSiteLinks TestUserDetails where
  rootLink = RootLink Nothing
  registerLink = RegisterLink
  userDetailsLink = UserDetailsLink
  emailConfirmLink = EmailConfirmLink
  getUserDetailsLink link = case link of
    UserDetailsLink mDetails -> Just mDetails
    _ -> Nothing
  toDocumentTitle _ = "" -- FIXME how to get blog title from permalink?
  subsidiaryTitle _ = " Blog"
  breadcrumb siteLink = case siteLink of
    NewBlogPostLink -> Just $ NonEmpty (RootLink Nothing) []
    _ -> Nothing


-- Policy: don't fail on bad query params / fragment unless you have to
instance fromLocationTestSiteLinks :: FromLocation TestSiteLinks where
  fromLocation (Location path mQuery mFrag) = do
    case runParser siteLinksPathParser (URIPath.printPath path) of
      Left e -> Left (show e)
      Right link -> pure link

siteLinksPathParser :: Parser TestSiteLinks
siteLinksPathParser = do
  divider
  let blogPost = RootLink <<< Just <$> permalinkParser
      def = defaultSiteLinksPathParser userDetailsLinksParser (Just blogPost)
      register = RegisterLink <$ string "register"
      newBlogPost = NewBlogPostLink <$ string "newBlogPost"
      emailConfirm = EmailConfirmLink <$ string "emailConfirm"
  try emailConfirm
    <|> try register
    <|> try newBlogPost
    <|> def
  where
    divider = void (char '/')



main :: Eff _ Unit
main = do
  log "JSON Iso:"
  jsonIsoAssert "Data.Address" (Proxy :: Proxy USAAddress)
  jsonIsoAssert "Data.Image.Source" (Proxy :: Proxy ImageSource)
  jsonIsoAssert "Data.List.Pagination" (Proxy :: Proxy PaginationArgs)
  jsonIsoAssert "Data.List.Sorting" (Proxy :: Proxy (SortingArgs Unit))
  jsonIsoAssert "Data.Price" (Proxy :: Proxy Price)
  jsonIsoAssert "Data.String.Markdown" (Proxy :: Proxy MarkdownText)
  jsonIsoAssert "Data.String.Permalink" (Proxy :: Proxy Permalink)
  jsonIsoAssert "Data.Date.JSON" (Proxy :: Proxy JSONDate)
  jsonIsoAssert "Data.DateTime.JSON" (Proxy :: Proxy JSONDateTime)
  log "--------"
  jsonIsoAssert "LocalCooking.Common.AccessToken" (Proxy :: Proxy AccessToken)
  jsonIsoAssert "LocalCooking.Common.Ingredient" (Proxy :: Proxy Ingredient)
  jsonIsoAssert "LocalCooking.Common.Order" (Proxy :: Proxy OrderProgress)
  jsonIsoAssert "LocalCooking.Common.Rating" (Proxy :: Proxy Rating)
  jsonIsoAssert "LocalCooking.Common.Tag" (Proxy :: Proxy Tag)
  jsonIsoAssert "LocalCooking.Common.User.Name" (Proxy :: Proxy Name)
  jsonIsoAssert "LocalCooking.Common.User.Password" (Proxy :: Proxy HashedPassword)
  jsonIsoAssert "LocalCooking.Common.User.Role" (Proxy :: Proxy UserRole)
  log "--------"
  jsonIsoAssert "LocalCooking.Semantics.Common.SocialLoginForm" (Proxy :: Proxy SocialLoginForm)
  jsonIsoAssert "LocalCooking.Semantics.Common.User" (Proxy :: Proxy User)
  jsonIsoAssert "LocalCooking.Semantics.Common.SetUser" (Proxy :: Proxy SetUser)
  jsonIsoAssert "LocalCooking.Semantics.Common.Register" (Proxy :: Proxy Register)
  jsonIsoAssert "LocalCooking.Semantics.Common.RegisterError" (Proxy :: Proxy RegisterError)
  jsonIsoAssert "LocalCooking.Semantics.Common.Login" (Proxy :: Proxy Login)
  jsonIsoAssert "LocalCooking.Semantics.Common.SocialLogin" (Proxy :: Proxy SocialLogin)
  log "--------"
  jsonIsoAssert "LocalCooking.Semantics.Mitch.ReviewSynopsis" (Proxy :: Proxy Mitch.ReviewSynopsis)
  jsonIsoAssert "LocalCooking.Semantics.Mitch.Review" (Proxy :: Proxy Mitch.Review)
  jsonIsoAssert "LocalCooking.Semantics.Mitch.MealSynopsis" (Proxy :: Proxy Mitch.MealSynopsis)
  jsonIsoAssert "LocalCooking.Semantics.Mitch.Meal" (Proxy :: Proxy Mitch.Meal)
  jsonIsoAssert "LocalCooking.Semantics.Mitch.MenuSynopsis" (Proxy :: Proxy Mitch.MenuSynopsis)
  jsonIsoAssert "LocalCooking.Semantics.Mitch.Menu" (Proxy :: Proxy Mitch.Menu)
  jsonIsoAssert "LocalCooking.Semantics.Mitch.ChefSynopsis" (Proxy :: Proxy Mitch.ChefSynopsis)
  jsonIsoAssert "LocalCooking.Semantics.Mitch.Chef" (Proxy :: Proxy Mitch.Chef)
  jsonIsoAssert "LocalCooking.Semantics.Mitch.Order" (Proxy :: Proxy Mitch.Order)
  log "--------"
  jsonIsoAssert "LocalCooking.Semantics.Chef.MealSettings" (Proxy :: Proxy Chef.MealSettings)
  jsonIsoAssert "LocalCooking.Semantics.Chef.MenuSettings" (Proxy :: Proxy Chef.MenuSettings)
  jsonIsoAssert "LocalCooking.Semantics.Chef.GetSetChef" (Proxy :: Proxy Chef.GetSetChef)
  jsonIsoAssert "LocalCooking.Semantics.Chef.Order" (Proxy :: Proxy Chef.Order)
  log "--------"
  jsonIsoAssert "LocalCooking.Semantics.Admin.SetUser" (Proxy :: Proxy Admin.SetUser)
  jsonIsoAssert "LocalCooking.Semantics.Admin.NewUser" (Proxy :: Proxy Admin.NewUser)
  log "--------"
  jsonIsoAssert "LocalCooking.Global.Error.AuthTokenFailure" (Proxy :: Proxy AuthTokenFailure)
  log "--------"
  jsonIsoAssert "Facebook.State.FacebookLoginUnsavedFormData" (Proxy :: Proxy FacebookLoginUnsavedFormData)
  jsonIsoAssert "Facebook.State.FacebookLoginState" (Proxy :: Proxy FacebookLoginState)
  log "\n-------------------------"
  log "Location Iso:"
  locationIsoAssert "TestSiteLinks" (Proxy :: Proxy TestSiteLinks)


locationIsoAssert :: forall a
               . ToLocation a
              => FromLocation a
              => Eq a
              => Show a
              => Arbitrary a
              => String -> Proxy a -> Eff _ Unit
locationIsoAssert name Proxy = do
  log ("    " <> name)
  quickCheck (\(x :: a) -> locationIso x)
  log ""


locationIso :: forall a. ToLocation a => FromLocation a => Eq a => Show a => a -> Result
locationIso x = case fromLocation (toLocation x) of
  Left y -> Failed $ "decoding failure: " <> y <> ", " <> show y
  Right y
    | x == y -> Success
    | otherwise -> Failed $ "Not identical: " <> show x <> ", " <> show y


jsonIsoAssert :: forall a
               . EncodeJson a
              => DecodeJson a
              => Eq a
              => Show a
              => Arbitrary a
              => String -> Proxy a -> Eff _ Unit
jsonIsoAssert name Proxy = do
  log ("    " <> name)
  quickCheck (\(x :: a) -> jsonIso x)
  log ""


jsonIso :: forall a. EncodeJson a => DecodeJson a => Eq a => Show a => a -> Result
jsonIso x = case decodeJson (encodeJson x) of
  Left y -> Failed $ "decoding failure: " <> y <> ", " <> show y
  Right y
    | x == y -> Success
    | otherwise -> Failed $ "Not identical: " <> show x <> ", " <> show y
