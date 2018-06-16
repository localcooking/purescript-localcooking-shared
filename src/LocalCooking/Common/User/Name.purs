module LocalCooking.Common.User.Name where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Generic (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, fail)
import Data.NonEmpty (NonEmpty (..))
import Data.Array (uncons) as Array
import Data.String.Yarn (words, unwords) as String
import Data.String.NonEmptyAlpha (NonEmptyAlphaString (..))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (arrayOf1)


newtype Name = Name (NonEmpty Array String)

derive instance genericName :: Generic Name
derive newtype instance eqName :: Eq Name
derive newtype instance ordName :: Ord Name
-- derive newtype instance encodeJsonName :: EncodeJson Name
-- derive newtype instance decodeJsonName :: DecodeJson Name

instance arbitraryName :: Arbitrary Name where
  arbitrary = do
    ns <- arrayOf1 $ do
      NonEmptyAlphaString x <- arbitrary
      pure x
    pure (Name ns)

instance showName :: Show Name where
  show (Name (NonEmpty n ns)) = String.unwords ([n] <> ns)

instance encodeJsonName :: EncodeJson Name where
  encodeJson (Name (NonEmpty n ns)) = encodeJson ([n] <> ns)

instance decodeJsonName :: DecodeJson Name where
  decodeJson json = do
    as <- decodeJson json
    case Array.uncons as of
      Nothing -> fail "Empty Name"
      Just {head,tail} -> pure $ Name $ NonEmpty head tail

name :: String -> Maybe Name
name x =
  case Array.uncons (String.words x) of
    Nothing -> Nothing
    Just {head,tail} -> Just $ Name $ NonEmpty head tail
