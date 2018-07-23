module LocalCooking.Common.Blog where

import Prelude
import Data.NonEmpty (NonEmpty (..))
import Data.Generic (class Generic, gShow, gEq, gCompare)
import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, encodeJson, fail)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)


data BlogPostVariant
  = CasualBlogPost
  | BusinessBlogPost
  | PersonalBlogPost

derive instance genericBlogPostVariant :: Generic BlogPostVariant

instance arbitraryBlogPostVariant :: Arbitrary BlogPostVariant where
  arbitrary = oneOf $ NonEmpty
    ( pure CasualBlogPost)
    [ pure BusinessBlogPost
    , pure PersonalBlogPost
    ]

instance eqBlogPostVariant :: Eq BlogPostVariant where
  eq = gEq

instance ordBlogPostVariant :: Ord BlogPostVariant where
  compare = gCompare

-- TODO Enum, Bounded, BoundedEnum

instance showBlogPostVariant :: Show BlogPostVariant where
  show = gShow

instance encodeJsonBlogPostVariant :: EncodeJson BlogPostVariant where
  encodeJson x = encodeJson $ case x of
    CasualBlogPost -> "casual"
    BusinessBlogPost -> "business"
    PersonalBlogPost -> "personal"

instance decodeJsonBlogPostVariant :: DecodeJson BlogPostVariant where
  decodeJson json = do
    s <- decodeJson json
    case unit of
      _ | s == "casual" -> pure CasualBlogPost
        | s == "business" -> pure BusinessBlogPost
        | s == "personal" -> pure PersonalBlogPost
        | otherwise -> fail "Not a BlogPostVariant"



newtype BlogPostPriority = BlogPostPriority Int

derive instance genericBlogPostPriority :: Generic BlogPostPriority

instance eqBlogPostPriority :: Eq BlogPostPriority where
  eq = gEq

instance ordBlogPostPriority :: Ord BlogPostPriority where
  compare = gCompare

-- TODO Enum

instance arbitraryBlogPostPriority :: Arbitrary BlogPostPriority where
  arbitrary = BlogPostPriority <$> arbitrary

instance encodeJsonBlogPostPriority :: EncodeJson BlogPostPriority where
  encodeJson (BlogPostPriority x) = encodeJson x

instance decodeJsonBlogPostPriority :: DecodeJson BlogPostPriority where
  decodeJson json = BlogPostPriority <$> (decodeJson json)


newtype BlogPostCategory = BlogPostCategory String

derive instance genericBlogPostCategory :: Generic BlogPostCategory

instance eqBlogPostCategory :: Eq BlogPostCategory where
  eq = gEq

instance ordBlogPostCategory :: Ord BlogPostCategory where
  compare = gCompare

-- TODO Enum

instance arbitraryBlogPostCategory :: Arbitrary BlogPostCategory where
  arbitrary = BlogPostCategory <$> arbitrary

instance encodeJsonBlogPostCategory :: EncodeJson BlogPostCategory where
  encodeJson (BlogPostCategory x) = encodeJson x

instance decodeJsonBlogPostCategory :: DecodeJson BlogPostCategory where
  decodeJson json = BlogPostCategory <$> (decodeJson json)



