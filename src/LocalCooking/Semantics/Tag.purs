module LocalCooking.Semantics.Tag where

import Prelude
import Data.Maybe (Maybe)
import Data.NonEmpty (NonEmpty (..))
import Data.Generic (class Generic, gEq, gShow)
import Data.Argonaut
  ( class EncodeJson, class DecodeJson, encodeJson, decodeJson
  , fail, (:=), (~>), jsonEmptyObject, (.?))
import Control.Alternative ((<|>))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)


data TagExists a
  = TagDoesntExist
  | TagExists a

derive instance genericTagExists :: Generic a => Generic (TagExists a)

instance arbitraryTagExists :: Arbitrary a => Arbitrary (TagExists a) where
  arbitrary = oneOf $ NonEmpty
    ( pure TagDoesntExist
    )
    [ TagExists <$> arbitrary
    ]

instance eqTagExists :: Generic a => Eq (TagExists a) where
  eq = gEq

instance showTagExists :: Generic a => Show (TagExists a) where
  show = gShow

instance encodeJsonTagExists :: EncodeJson a => EncodeJson (TagExists a) where
  encodeJson x = case x of
    TagDoesntExist -> encodeJson "tagDoesntExist"
    TagExists y
      -> "tagExists"
      := y
      ~> jsonEmptyObject

instance decodeJsonTagExists :: DecodeJson a => DecodeJson (TagExists a) where
  decodeJson json = do
    let empty = do
          s <- decodeJson json
          if s == "tagDoesntExist"
             then pure TagDoesntExist
             else fail "Not a TagExists"
        has = do
          o <- decodeJson json
          TagExists <$> o .? "tagExists"
    empty <|> has

