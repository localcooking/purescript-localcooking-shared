module LocalCooking.Semantics.Content.Approval where

import LocalCooking.Semantics.ContentRecord (ContentRecord)
import LocalCooking.Semantics.ContentRecord.Variant (ContentRecordVariant)
import LocalCooking.Database.Schema (RecordSubmissionApprovalId, StoredUserId, StoredEditorId)

import Prelude
import Data.Name (Name)
import Data.Generic (class Generic, gEq, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, (:=), (~>), jsonEmptyObject, (.?))
import Data.Argonaut.JSONDateTime (JSONDateTime (..))
import Data.DateTime (DateTime)
import Test.QuickCheck (class Arbitrary, arbitrary)


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



newtype GetRecordSubmission = GetRecordSubmission
  { author :: StoredUserId
  , timestamp :: DateTime
  , submission :: ContentRecord
  , approvals :: Array StoredEditorId
  }

derive instance genericGetRecordSubmission :: Generic GetRecordSubmission

instance eqGetRecordSubmission :: Eq GetRecordSubmission where
  eq = gEq

instance showGetRecordSubmission :: Show GetRecordSubmission where
  show = gShow

instance arbitraryGetRecordSubmission :: Arbitrary GetRecordSubmission where
  arbitrary = do
    author <- arbitrary
    JSONDateTime timestamp <- arbitrary
    submission <- arbitrary
    approvals <- arbitrary
    pure (GetRecordSubmission {author,timestamp,submission,approvals})

instance encodeJsonGetRecordSubmission :: EncodeJson GetRecordSubmission where
  encodeJson (GetRecordSubmission {author,timestamp,submission,approvals})
    =  "author" := author
    ~> "timestamp" := JSONDateTime timestamp
    ~> "submission" := submission
    ~> "approvals" := approvals
    ~> jsonEmptyObject

instance decodeJsonGetRecordSubmission :: DecodeJson GetRecordSubmission where
  decodeJson json = do
    o <- decodeJson json
    author <- o .? "arbitrary"
    JSONDateTime timestamp <- o .? "timestamp"
    submission <- o .? "submission"
    approvals <- o .? "approvals"
    pure (GetRecordSubmission {author,timestamp,submission,approvals})
