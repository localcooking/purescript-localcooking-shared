module LocalCooking.Common.Blog where

import Prelude
import Data.Either (Either (..))
import Data.Maybe (Maybe (..))
import Data.NonEmpty (NonEmpty (..))
import Data.Generic (class Generic, gEq, gCompare)
import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, encodeJson, fail)
import Data.Enum (class Enum, class BoundedEnum, Cardinality (..))
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

instance enumBlogPostVariant :: Enum BlogPostVariant where
  succ x = case x of
    CasualBlogPost -> Just BusinessBlogPost
    BusinessBlogPost -> Just PersonalBlogPost
    PersonalBlogPost -> Nothing
  pred x = case x of
    CasualBlogPost -> Nothing
    BusinessBlogPost -> Just CasualBlogPost
    PersonalBlogPost -> Just BusinessBlogPost

instance boundedBlogPostVariant :: Bounded BlogPostVariant where
  top = PersonalBlogPost
  bottom = CasualBlogPost

instance boundedEnumBlogPostVariant :: BoundedEnum BlogPostVariant where
  cardinality = Cardinality 3
  toEnum i
    | i == 0 = Just CasualBlogPost
    | i == 1 = Just BusinessBlogPost
    | i == 2 = Just PersonalBlogPost
    | otherwise = Nothing
  fromEnum x = case x of
    CasualBlogPost -> 0
    BusinessBlogPost -> 1
    PersonalBlogPost -> 2


-- TODO Enum, Bounded, BoundedEnum

instance showBlogPostVariant :: Show BlogPostVariant where
  show x = case x of
    CasualBlogPost -> "Casual"
    BusinessBlogPost -> "Business"
    PersonalBlogPost -> "Personal"

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



