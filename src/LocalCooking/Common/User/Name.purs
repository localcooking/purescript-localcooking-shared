module LocalCooking.Common.User.Name where

import Prelude
import Data.Generic (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.NonEmpty (NonEmpty (..))
import Data.String.Yarn (words, unwords) as String
import Data.String.NonEmptyAlpha (NonEmptyAlphaString (..))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (arrayOf1)


newtype Name = Name (Array String)

derive instance genericName :: Generic Name
derive newtype instance eqName :: Eq Name
derive newtype instance ordName :: Ord Name
derive newtype instance encodeJsonName :: EncodeJson Name
derive newtype instance decodeJsonName :: DecodeJson Name

instance arbitraryName :: Arbitrary Name where
  arbitrary = do
    NonEmpty n ns <- arrayOf1 $ do
      NonEmptyAlphaString x <- arbitrary
      pure x
    pure $ Name $ [n] <> ns

instance showName :: Show Name where
  show (Name ns) = String.unwords ns

name :: String -> Name
name = Name <<< String.words
