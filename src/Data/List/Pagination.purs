module Data.List.Pagination where

import Prelude
import Data.NonEmpty (NonEmpty (..))
import Data.Generic (class Generic, gEq, gCompare, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, fail, (:=), (~>), jsonEmptyObject, (.?))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)


newtype PaginationArgs = PaginationArgs
  { index :: Int
  , size  :: Int
  }

derive instance genericPaginationArgs :: Generic PaginationArgs

instance arbitraryPaginationArgs :: Arbitrary PaginationArgs where
  arbitrary = do
    index <- arbitrary
    size <- arbitrary
    pure (PaginationArgs {index,size})

instance eqPaginationArgs :: Eq PaginationArgs where
  eq = gEq

instance showPaginationArgs :: Show PaginationArgs where
  show = gShow

instance encodeJsonPaginationArgs :: EncodeJson PaginationArgs where
  encodeJson (PaginationArgs {index,size})
    =  "index" := index
    ~> "size" := size
    ~> jsonEmptyObject

instance decodeJsonPaginationArgs :: DecodeJson PaginationArgs where
  decodeJson json = do
    o <- decodeJson json
    index <- o .? "index"
    size <- o .? "size"
    pure (PaginationArgs {index,size})

