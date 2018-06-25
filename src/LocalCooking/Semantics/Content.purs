module LocalCooking.Semantics.Content where

import LocalCooking.Semantics.ContentRecord (ContentRecordVariant, ContentRecord)
import LocalCooking.Database.Schema (RecordSubmissionApprovalId, StoredUserId, StoredEditorId)
import LocalCooking.Common.User.Name (Name)

import Prelude
import Data.Generic (class Generic, gEq, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, (:=), (~>), jsonEmptyObject, (.?))
import Data.DateTime (DateTime)
import Data.DateTime.JSON (JSONDateTime (..))
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




newtype SetEditor = SetEditor
  { name :: Name
  }

derive instance genericSetEditor :: Generic SetEditor

instance eqSetEditor :: Eq SetEditor where
  eq = gEq

instance showSetEditor :: Show SetEditor where
  show = gShow

instance arbitrarySetEditor :: Arbitrary SetEditor where
  arbitrary = do
    name <- arbitrary
    pure (SetEditor {name})

instance encodeJsonSetEditor :: EncodeJson SetEditor where
  encodeJson (SetEditor {name})
    =  "name" := name
    ~> jsonEmptyObject

instance decodeJsonSetEditor :: DecodeJson SetEditor where
  decodeJson json = do
    o <- decodeJson json
    name <- o .? "name"
    pure (SetEditor {name})



newtype GetRecordSubmissionPolicy = GetRecordSubmissionPolicy
  { variant :: ContentRecordVariant
  , additional :: Int
  , assigned :: Array StoredEditorId
  }

derive instance genericGetRecordSubmissionPolicy :: Generic GetRecordSubmissionPolicy

instance eqGetRecordSubmissionPolicy :: Eq GetRecordSubmissionPolicy where
  eq = gEq

instance showGetRecordSubmissionPolicy :: Show GetRecordSubmissionPolicy where
  show = gShow

instance arbitraryGetRecordSubmissionPolicy :: Arbitrary GetRecordSubmissionPolicy where
  arbitrary = do
    variant <- arbitrary
    additional <- arbitrary
    assigned <- arbitrary
    pure (GetRecordSubmissionPolicy {variant,additional,assigned})

instance encodeJsonGetRecordSubmissionPolicy :: EncodeJson GetRecordSubmissionPolicy where
  encodeJson (GetRecordSubmissionPolicy {variant,additional,assigned})
    =  "variant" := variant
    ~> "additional" := additional
    ~> "assigned" := assigned
    ~> jsonEmptyObject

instance decodeJsonGetRecordSubmissionPolicy :: DecodeJson GetRecordSubmissionPolicy where
  decodeJson json = do
    o <- decodeJson json
    variant <- o .? "variant"
    additional <- o .? "additional"
    assigned <- o .? "assigned"
    pure (GetRecordSubmissionPolicy {variant,additional,assigned})



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
