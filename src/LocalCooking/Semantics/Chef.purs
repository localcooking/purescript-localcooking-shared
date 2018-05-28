module LocalCooking.Semantics.Chef where

-- import LocalCooking.Database.Schema (StoredUserId)
-- import LocalCooking.Common.User.Role (UserRole)
import LocalCooking.Common.User.Name (Name)
import LocalCooking.Common.Tag.Chef (ChefTag)
-- import Facebook.Types (FacebookUserId, FacebookLoginCode)
-- import Google.ReCaptcha (ReCaptchaResponse)

import Prelude
import Data.String.Markdown (MarkdownText)
import Data.String.Permalink (Permalink)
import Data.Image.Source (ImageSource)
import Data.DateTime (DateTime)
import Data.DateTime.JSON (JSONDateTime (..))
import Data.DateTime.Locale (LocalValue (..))
import Data.Maybe (Maybe)
import Data.Generic (class Generic, gEq, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, fail, (:=), (~>), jsonEmptyObject, (.?))
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Now (nowDateTime)
import Text.Email.Validate (EmailAddress)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)


newtype ChefSettings = ChefSettings
  { name      :: Name
  , permalink :: Permalink
  , images    :: Array ImageSource
  , avatar    :: ImageSource
  , bio       :: MarkdownText
  , tags      :: Array ChefTag
  }

derive instance genericChefSettings :: Generic ChefSettings
