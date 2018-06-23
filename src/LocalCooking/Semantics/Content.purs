module LocalCooking.Semantics.Content where

import LocalCooking.Common.User.Name (Name)
import LocalCooking.Common.ContentRecord (ContentRecordVariant)
import LocalCooking.Database.Schema (RecordSubmissionApprovalId)

import Prelude
import Data.Maybe (Maybe)
import Data.NonEmpty (NonEmpty (..))
import Data.Generic (class Generic, gEq, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, (:=), (~>), jsonEmptyObject, (.?))
import Control.Alternative ((<|>))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)



newtype GetEditor = GetEditor
  { name :: Name
  , assignedRecords :: Array ContentRecordVariant
  , approvedSubmissions :: Array RecordSubmissionApprovalId
  }

derive instance genericGetEditor :: Generic GetEditor

instance eqGetEditor :: Eq GetEditor where
  eq = gEq

instance showGetEditor :: Show GetEditor where
  show = gShow

instance arbitraryGetEditor :: Arbitrary GetEditor where
  arbitrary = do
    name <- arbitrary
    assignedRecords <- arbitrary
    approvedSubmissions <- arbitrary
    pure (GetEditor {name,assignedRecords,approvedSubmissions})

instance encodeJsonGetEditor :: EncodeJson GetEditor where
  encodeJson (GetEditor {name,assignedRecords,approvedSubmissions})
    =  "name" := name
    ~> "assignedRecords" := assignedRecords
    ~> "approvedSubmissions" := approvedSubmissions
    ~> jsonEmptyObject

instance decodeJsonGetEditor :: DecodeJson GetEditor where
  decodeJson json = do
    o <- decodeJson json
    name <- o .? "name"
    assignedRecords <- o .? "assignedRecords"
    approvedSubmissions <- o .? "approvedSubmissions"
    pure (GetEditor {name,assignedRecords,approvedSubmissions})
