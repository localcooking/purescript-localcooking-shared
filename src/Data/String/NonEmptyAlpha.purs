module Data.String.NonEmptyAlpha where

import Prelude
import Data.Generic (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.NonEmpty (NonEmpty (..))
import Data.Enum (enumFromTo)
import Data.String.Yarn (fromChars) as String
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (arrayOf1, elements)


newtype NonEmptyAlphaString = NonEmptyAlphaString String

derive instance genericNonEmptyAlphaString :: Generic NonEmptyAlphaString
derive newtype instance eqNonEmptyAlphaString :: Eq NonEmptyAlphaString
derive newtype instance ordNonEmptyAlphaString :: Ord NonEmptyAlphaString
derive newtype instance showNonEmptyAlphaString :: Show NonEmptyAlphaString
derive newtype instance encodeJsonNonEmptyAlphaString :: EncodeJson NonEmptyAlphaString
derive newtype instance decodeJsonNonEmptyAlphaString :: DecodeJson NonEmptyAlphaString

instance arbitraryNonEmptyAlphaString :: Arbitrary NonEmptyAlphaString where
  arbitrary = do
    cs <- arrayOf1 $ elements $ NonEmpty 'a' (enumFromTo 'b' 'c')
    pure $ NonEmptyAlphaString $ String.fromChars cs
