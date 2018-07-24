module Text.Parsing.StringParser.Int where

import Prelude
import Text.Parsing.StringParser (Parser, try, fail)
import Text.Parsing.StringParser.String (anyDigit)
import Text.Parsing.StringParser.Combinators (many1)
import Data.Int.Parse (parseInt, toRadix)
import Data.String.Yarn (fromChars)
import Data.Maybe (Maybe (..))


int :: Parser Int
int = do
  xs <- fromChars <$> try (many1 anyDigit)
  case parseInt xs (toRadix 10) of
    Nothing -> fail "Not an Int"
    Just x -> pure x
