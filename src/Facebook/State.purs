module Facebook.State where

import LocalCooking.Semantics.Common (SocialLoginForm)

import Prelude
import Data.Maybe (Maybe)
import Data.Either (Either (..))
import Data.URI.Location (Location, parseLocation, printLocation)
import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, (:=), (~>), jsonEmptyObject, (.?), fail)
import Data.Generic (class Generic, gEq, gShow)
import Data.NonEmpty (NonEmpty (..))
import Control.Alternative ((<|>))
import Text.Parsing.StringParser (runParser)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)


-- TODO FIXME validate siteLinks origin?
newtype FacebookLoginState = FacebookLoginState
  { origin :: Location -- in-site
  , formData :: Maybe FacebookLoginUnsavedFormData
  }

derive instance genericFacebookLoginState :: Generic FacebookLoginState

instance eqFacebookLoginState :: Eq FacebookLoginState where
  eq = gEq

instance arbitraryFacebookLoginState :: Arbitrary FacebookLoginState where
  arbitrary = do
    origin <- arbitrary
    formData <- arbitrary
    pure $ FacebookLoginState {origin,formData}

instance encodeJsonFacebookLoginState :: EncodeJson FacebookLoginState where
  encodeJson (FacebookLoginState {origin,formData})
    =  "origin" := printLocation origin
    ~> "formData" := formData
    ~> jsonEmptyObject

instance decodeJsonFacebookLoginState :: DecodeJson FacebookLoginState where
  decodeJson json = do
    o <- decodeJson json
    formData <- o .? "formData"
    s <- o .? "origin"
    case runParser parseLocation s of
      Left e -> fail (show e)
      Right origin -> pure $ FacebookLoginState {origin,formData} -- case fromLocation loc of
        -- Left e -> fail e
        -- Right origin -> pure $ FacebookLoginState {origin,formData}



data FacebookLoginUnsavedFormData
  = FacebookLoginUnsavedFormDataRegister
    { email :: String
    , emailConfirm :: String
    , socialLogin :: SocialLoginForm
    }
  | FacebookLoginUnsavedFormDataSecurity
    { email :: String
    , emailConfirm :: String
    -- TODO FIXME socialLogin here as well
    }

derive instance genericFacebookLoginUnsavedFormData :: Generic FacebookLoginUnsavedFormData

instance showFacebookLoginUnsavedFormData :: Show FacebookLoginUnsavedFormData where
  show = gShow

instance arbitraryFacebookLoginUnsavedFormData :: Arbitrary FacebookLoginUnsavedFormData where
  arbitrary = oneOf $ NonEmpty
    ( do email <- arbitrary
         emailConfirm <- arbitrary
         socialLogin <- arbitrary
         pure $ FacebookLoginUnsavedFormDataRegister {email,emailConfirm,socialLogin}
    )
    [ do email <- arbitrary
         emailConfirm <- arbitrary
         pure $ FacebookLoginUnsavedFormDataSecurity {email,emailConfirm}
    ]

instance encodeJsonFacebookLoginUnsavedFormData :: EncodeJson FacebookLoginUnsavedFormData where
  encodeJson x = case x of
    FacebookLoginUnsavedFormDataRegister {email,emailConfirm,socialLogin}
      -> "register" :=
         ( "email" := email
         ~> "emailConfirm" := emailConfirm
         ~> "socialLogin" := socialLogin
         ~> jsonEmptyObject )
      ~> jsonEmptyObject
    FacebookLoginUnsavedFormDataSecurity {email,emailConfirm}
      -> "security" :=
         ( "email" := email
         ~> "emailConfirm" := emailConfirm
         ~> jsonEmptyObject )
      ~> jsonEmptyObject

instance decodeJsonFacebookLoginUnsavedFormData :: DecodeJson FacebookLoginUnsavedFormData where
  decodeJson json = do
    o <- decodeJson json
    let register = do
          o' <- o .? "register"
          email <- o' .? "email"
          emailConfirm <- o' .? "emailConfirm"
          socialLogin <- o' .? "socialLogin"
          pure (FacebookLoginUnsavedFormDataRegister {email,emailConfirm,socialLogin})
        security = do
          o' <- o .? "security"
          email <- o' .? "email"
          emailConfirm <- o' .? "emailConfirm"
          pure (FacebookLoginUnsavedFormDataSecurity {email,emailConfirm})
    register <|> security
