module LocalCooking.Common.Blog where

import Prelude
import Data.Either (Either (..))
import Data.NonEmpty (NonEmpty (..))
import Data.Generic (class Generic, gShow, gEq, gCompare)
import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, encodeJson, fail)
import Control.Alternative ((<|>))
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.String (string)
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

printBlogPostVariant :: BlogPostVariant -> String
printBlogPostVariant x = case x of
  CasualBlogPost -> "casual"
  BusinessBlogPost -> "business"
  PersonalBlogPost -> "personal"

instance encodeJsonBlogPostVariant :: EncodeJson BlogPostVariant where
  encodeJson = encodeJson <<< printBlogPostVariant

instance decodeJsonBlogPostVariant :: DecodeJson BlogPostVariant where
  decodeJson json = do
    s <- decodeJson json
    case runParser blogPostVariantParser s of
      Left e -> fail $ "Not a BlogPostVariant: " <> show e
      Right x -> pure x

blogPostVariantParser :: Parser BlogPostVariant
blogPostVariantParser = do
  let casual = CasualBlogPost <$ string "casual"
      business = BusinessBlogPost <$ string "business"
      personal = PersonalBlogPost <$ string "personal"
  casual <|> business <|> personal


newtype BlogPostPriority = BlogPostPriority Int

derive instance genericBlogPostPriority :: Generic BlogPostPriority

instance eqBlogPostPriority :: Eq BlogPostPriority where
  eq = gEq

instance ordBlogPostPriority :: Ord BlogPostPriority where
  compare = gCompare

instance showBlogPostPriority :: Show BlogPostPriority where
  show (BlogPostPriority i) = show i

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

instance showBlogPostCategory :: Show BlogPostCategory where
  show (BlogPostCategory x) = x

-- TODO Enum

instance arbitraryBlogPostCategory :: Arbitrary BlogPostCategory where
  arbitrary = BlogPostCategory <$> arbitrary

instance encodeJsonBlogPostCategory :: EncodeJson BlogPostCategory where
  encodeJson (BlogPostCategory x) = encodeJson x

instance decodeJsonBlogPostCategory :: DecodeJson BlogPostCategory where
  decodeJson json = BlogPostCategory <$> (decodeJson json)



