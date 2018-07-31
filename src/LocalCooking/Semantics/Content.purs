module LocalCooking.Semantics.Content where

import LocalCooking.Semantics.ContentRecord.Variant (ContentRecordVariant)
import LocalCooking.Database.Schema (StoredEditorId)

import Prelude
import Data.Name (Name)
import Data.Maybe (Maybe)
import Data.Generic (class Generic, gEq, gShow)
import Data.NonEmpty (NonEmpty (..))
import Data.Argonaut
  (class EncodeJson, class DecodeJson, encodeJson, decodeJson
  , fail, (:=), (~>), jsonEmptyObject, (.?))
import Control.Alternative ((<|>))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)





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


-- * Errors


data SubmissionPolicy a
  = NoSubmissionPolicy
  | SubmissionPolicy a

derive instance genericSubmissionPolicy :: Generic a => Generic (SubmissionPolicy a)

instance arbitrarySubmissionPolicy :: Arbitrary a => Arbitrary (SubmissionPolicy a) where
  arbitrary = oneOf $ NonEmpty
    ( pure NoSubmissionPolicy
    )
    [ SubmissionPolicy <$> arbitrary
    ]

instance eqSubmissionPolicy :: Generic a => Eq (SubmissionPolicy a) where
  eq = gEq

instance showSubmissionPolicy :: Generic a => Show (SubmissionPolicy a) where
  show = gShow

instance encodeJsonSubmissionPolicy :: EncodeJson a => EncodeJson (SubmissionPolicy a) where
  encodeJson x = case x of
    NoSubmissionPolicy -> encodeJson "noSubmissionPolicy"
    SubmissionPolicy y
      -> "submissionPolicy"
      := y
      ~> jsonEmptyObject

instance decodeJsonSubmissionPolicy :: DecodeJson a => DecodeJson (SubmissionPolicy a) where
  decodeJson json = do
    let empty = do
          s <- decodeJson json
          if s == "noSubmissionPolicy"
             then pure NoSubmissionPolicy
             else fail "Not a SubmissionPolicy"
        has = do
          o <- decodeJson json
          SubmissionPolicy <$> o .? "submissionPolicy"
    empty <|> has


data SubmissionExists a
  = SubmissionDoesntExist
  | SubmissionExists a

derive instance genericSubmissionExists :: Generic a => Generic (SubmissionExists a)

instance arbitrarySubmissionExists :: Arbitrary a => Arbitrary (SubmissionExists a) where
  arbitrary = oneOf $ NonEmpty
    ( pure SubmissionDoesntExist
    )
    [ SubmissionExists <$> arbitrary
    ]

instance eqSubmissionExists :: Generic a => Eq (SubmissionExists a) where
  eq = gEq

instance showSubmissionExists :: Generic a => Show (SubmissionExists a) where
  show = gShow

instance encodeJsonSubmissionExists :: EncodeJson a => EncodeJson (SubmissionExists a) where
  encodeJson x = case x of
    SubmissionDoesntExist -> encodeJson "submissionDoesntExist"
    SubmissionExists y
      -> "submissionExists"
      := y
      ~> jsonEmptyObject

instance decodeJsonSubmissionExists :: DecodeJson a => DecodeJson (SubmissionExists a) where
  decodeJson json = do
    let empty = do
          s <- decodeJson json
          if s == "submissionDoesntExist"
             then pure SubmissionDoesntExist
             else fail "Not a SubmissionExists"
        has = do
          o <- decodeJson json
          SubmissionExists <$> o .? "submissionExists"
    empty <|> has


data EditorExists a
  = EditorDoesntExist
  | EditorExists a

derive instance genericEditorExists :: Generic a => Generic (EditorExists a)

instance arbitraryEditorExists :: Arbitrary a => Arbitrary (EditorExists a) where
  arbitrary = oneOf $ NonEmpty
    ( pure EditorDoesntExist
    )
    [ EditorExists <$> arbitrary
    ]

instance eqEditorExists :: Generic a => Eq (EditorExists a) where
  eq = gEq

instance showEditorExists :: Generic a => Show (EditorExists a) where
  show = gShow

instance encodeJsonEditorExists :: EncodeJson a => EncodeJson (EditorExists a) where
  encodeJson x = case x of
    EditorDoesntExist -> encodeJson "editorDoesntExist"
    EditorExists y
      -> "editorExists"
      := y
      ~> jsonEmptyObject

instance decodeJsonEditorExists :: DecodeJson a => DecodeJson (EditorExists a) where
  decodeJson json = do
    let empty = do
          s <- decodeJson json
          if s == "editorDoesntExist"
             then pure EditorDoesntExist
             else fail "Not a EditorExists"
        has = do
          o <- decodeJson json
          EditorExists <$> o .? "editorExists"
    empty <|> has
