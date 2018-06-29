module Data.String.Permalink where

import Prelude
import Data.Generic (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.String.Yarn (class IsString, fromChars)
import Control.Alternative ((<|>))
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.String (alphaNum, char)
import Text.Parsing.StringParser.Combinators (many1)
import Test.QuickCheck (class Arbitrary)


newtype Permalink = Permalink String

derive instance genericPermalink :: Generic Permalink
derive newtype instance arbitraryPermalink :: Arbitrary Permalink
derive newtype instance eqPermalink :: Eq Permalink
derive newtype instance ordPermalink :: Ord Permalink
derive newtype instance encodeJsonPermalink :: EncodeJson Permalink
derive newtype instance decodeJsonPermalink :: DecodeJson Permalink
derive newtype instance isStringPermalink :: IsString Permalink

instance showPermalink :: Show Permalink where
  show (Permalink x) = x

permalinkParser :: Parser Permalink
permalinkParser = (Permalink <<< fromChars) <$> many1 (alphaNum <|> char '-' <|> char '_')
