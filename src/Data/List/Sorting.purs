module Data.List.Sorting where

import Prelude
import Data.NonEmpty (NonEmpty (..))
import Data.Generic (class Generic, gEq, gCompare, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, fail, (:=), (~>), jsonEmptyObject, (.?))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)


data SortOrdering = Ascending | Descending

derive instance genericSortOrdering :: Generic SortOrdering

instance arbitrarySortOrdering :: Arbitrary SortOrdering where
  arbitrary = oneOf $ NonEmpty
    ( pure Ascending
    )
    [ pure Descending
    ]

instance eqSortOrdering :: Eq SortOrdering where
  eq = gEq

instance ordSortOrdering :: Ord SortOrdering where
  compare = gCompare

instance showSortOrdering :: Show SortOrdering where
  show = gShow

instance encodeJsonSortOrdering :: EncodeJson SortOrdering where
  encodeJson x = encodeJson $ case x of
    Ascending -> "asc"
    Descending -> "des"

instance decodeJsonSortOrdering :: DecodeJson SortOrdering where
  decodeJson json = do
    s <- decodeJson json
    case unit of
      _ | s == "asc" -> pure Ascending
        | s == "des" -> pure Descending
        | otherwise  -> fail "SortOrdering"


newtype SortingArgs field = SortingArgs
  { ordering :: SortOrdering
  , field    :: field
  }

derive instance genericSortingArgs :: Generic field => Generic (SortingArgs field)

instance arbitrarySortingArgs :: Arbitrary field => Arbitrary (SortingArgs field) where
  arbitrary = do
    ordering <- arbitrary
    field <- arbitrary
    pure (SortingArgs {ordering,field})

instance eqSortingArgs :: Generic field => Eq (SortingArgs field) where
  eq = gEq

instance showSortingArgs :: Generic field => Show (SortingArgs field) where
  show = gShow

instance encodeJsonSortingArgs :: EncodeJson field => EncodeJson (SortingArgs field) where
  encodeJson (SortingArgs {ordering,field})
    =  "ordering" := ordering
    ~> "field" := field
    ~> jsonEmptyObject

instance decodeJsonSortingArgs :: DecodeJson field => DecodeJson (SortingArgs field) where
  decodeJson json = do
    o <- decodeJson json
    ordering <- o .? "ordering"
    field <- o .? "field"
    pure (SortingArgs {ordering,field})
