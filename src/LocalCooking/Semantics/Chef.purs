module LocalCooking.Semantics.Chef where

import LocalCooking.Database.Schema (StoredMealId, StoredOrderId)
import LocalCooking.Common.User.Name (Name)
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Common.Tag.Meal (MealTag)
import LocalCooking.Common.Ingredient (IngredientName)
import LocalCooking.Common.Order (OrderProgress)

import Prelude
import Data.Price (Price)
import Data.String.Markdown (MarkdownText)
import Data.String.Permalink (Permalink)
import Data.Image.Source (ImageSource)
import Data.Date (Date)
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


newtype MenuSettings = MenuSettings
  { published   :: Maybe Date
  , deadline    :: Date
  , heading     :: String
  , description :: MarkdownText
  , tags        :: Array MealTag
  , images      :: Array ImageSource
  }

derive instance genericMenuSettings :: Generic MenuSettings


newtype MealSettings = MealSettings
  { title        :: String
  , permalink    :: Permalink
  , heading      :: String
  , description  :: MarkdownText
  , instructions :: MarkdownText
  , images       :: Array ImageSource
  , ingredients  :: Array IngredientName
  , tags         :: Array MealTag
  , price        :: Price
  }

derive instance genericMealSettings :: Generic MealSettings


newtype Order = Order
  { meal     :: StoredMealId
  , progress :: OrderProgress
  , volume   :: Int
  , id       :: StoredOrderId
  }

derive instance genericOrder :: Generic Order
