module LocalCooking.Semantics.Content where

import LocalCooking.Semantics.ContentRecord.Variant (ContentRecordVariant)
import LocalCooking.Database.Schema (RecordSubmissionApprovalId, StoredUserId, StoredEditorId)
import LocalCooking.Common.User.Name (Name)

import Prelude
import Data.Maybe (Maybe)
import Data.Generic (class Generic, gEq, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, (:=), (~>), jsonEmptyObject, (.?))
import Data.DateTime (DateTime)
import Data.DateTime.JSON (JSONDateTime (..))
import Test.QuickCheck (class Arbitrary, arbitrary)





newtype SetEditor = SetEditor
  { name :: Maybe Name
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


newtype EditorValid = EditorValid
  { name :: Name
  }

derive instance genericEditorValid :: Generic EditorValid

instance eqEditorValid :: Eq EditorValid where
  eq = gEq

instance showEditorValid :: Show EditorValid where
  show = gShow

instance arbitraryEditorValid :: Arbitrary EditorValid where
  arbitrary = do
    name <- arbitrary
    pure (EditorValid {name})

instance encodeJsonEditorValid :: EncodeJson EditorValid where
  encodeJson (EditorValid {name})
    =  "name" := name
    ~> jsonEmptyObject

instance decodeJsonEditorValid :: DecodeJson EditorValid where
  decodeJson json = do
    o <- decodeJson json
    name <- o .? "name"
    pure (EditorValid {name})



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

