module Data.String.Markdown where

import Prelude
import Data.Generic (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Test.QuickCheck (class Arbitrary)


newtype MarkdownText = MarkdownText String

derive instance genericMarkdownText :: Generic MarkdownText
derive newtype instance arbitraryMarkdownText :: Arbitrary MarkdownText
derive newtype instance eqMarkdownText :: Eq MarkdownText
derive newtype instance ordMarkdownText :: Ord MarkdownText
derive newtype instance showMarkdownText :: Show MarkdownText
derive newtype instance encodeJsonMarkdownText :: EncodeJson MarkdownText
derive newtype instance decodeJsonMarkdownText :: DecodeJson MarkdownText
