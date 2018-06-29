module Data.String.Markdown where

import Prelude
import Data.Generic (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.String.Yarn (class IsString)
import Test.QuickCheck (class Arbitrary)


newtype MarkdownText = MarkdownText String

derive instance genericMarkdownText :: Generic MarkdownText
derive newtype instance arbitraryMarkdownText :: Arbitrary MarkdownText
derive newtype instance eqMarkdownText :: Eq MarkdownText
derive newtype instance ordMarkdownText :: Ord MarkdownText
derive newtype instance encodeJsonMarkdownText :: EncodeJson MarkdownText
derive newtype instance decodeJsonMarkdownText :: DecodeJson MarkdownText
derive newtype instance isStringMarkdownText :: IsString MarkdownText

instance showMarkdownText :: Show MarkdownText where
  show (MarkdownText x) = x
