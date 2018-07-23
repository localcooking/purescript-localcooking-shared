module LocalCooking.Database.Schema where

import Prelude
import Data.Generic (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Test.QuickCheck (class Arbitrary)


newtype StoredUserId = StoredUserId Int

derive instance genericStoredUserId :: Generic StoredUserId
derive newtype instance arbitraryStoredUserId :: Arbitrary StoredUserId
derive newtype instance eqStoredUserId :: Eq StoredUserId
derive newtype instance ordStoredUserId :: Ord StoredUserId
derive newtype instance showStoredUserId :: Show StoredUserId
derive newtype instance encodeJsonStoredUserId :: EncodeJson StoredUserId
derive newtype instance decodeJsonStoredUserId :: DecodeJson StoredUserId

newtype StoredReviewId = StoredReviewId Int

derive instance genericStoredReviewId :: Generic StoredReviewId
derive newtype instance arbitraryStoredReviewId :: Arbitrary StoredReviewId
derive newtype instance eqStoredReviewId :: Eq StoredReviewId
derive newtype instance ordStoredReviewId :: Ord StoredReviewId
derive newtype instance showStoredReviewId :: Show StoredReviewId
derive newtype instance encodeJsonStoredReviewId :: EncodeJson StoredReviewId
derive newtype instance decodeJsonStoredReviewId :: DecodeJson StoredReviewId

newtype StoredChefId = StoredChefId Int

derive instance genericStoredChefId :: Generic StoredChefId
derive newtype instance arbitraryStoredChefId :: Arbitrary StoredChefId
derive newtype instance eqStoredChefId :: Eq StoredChefId
derive newtype instance ordStoredChefId :: Ord StoredChefId
derive newtype instance showStoredChefId :: Show StoredChefId
derive newtype instance encodeJsonStoredChefId :: EncodeJson StoredChefId
derive newtype instance decodeJsonStoredChefId :: DecodeJson StoredChefId

newtype StoredMenuId = StoredMenuId Int

derive instance genericStoredMenuId :: Generic StoredMenuId
derive newtype instance arbitraryStoredMenuId :: Arbitrary StoredMenuId
derive newtype instance eqStoredMenuId :: Eq StoredMenuId
derive newtype instance ordStoredMenuId :: Ord StoredMenuId
derive newtype instance showStoredMenuId :: Show StoredMenuId
derive newtype instance encodeJsonStoredMenuId :: EncodeJson StoredMenuId
derive newtype instance decodeJsonStoredMenuId :: DecodeJson StoredMenuId

newtype StoredMealId = StoredMealId Int

derive instance genericStoredMealId :: Generic StoredMealId
derive newtype instance arbitraryStoredMealId :: Arbitrary StoredMealId
derive newtype instance eqStoredMealId :: Eq StoredMealId
derive newtype instance ordStoredMealId :: Ord StoredMealId
derive newtype instance showStoredMealId :: Show StoredMealId
derive newtype instance encodeJsonStoredMealId :: EncodeJson StoredMealId
derive newtype instance decodeJsonStoredMealId :: DecodeJson StoredMealId

newtype StoredOrderId = StoredOrderId Int

derive instance genericStoredOrderId :: Generic StoredOrderId
derive newtype instance arbitraryStoredOrderId :: Arbitrary StoredOrderId
derive newtype instance eqStoredOrderId :: Eq StoredOrderId
derive newtype instance ordStoredOrderId :: Ord StoredOrderId
derive newtype instance showStoredOrderId :: Show StoredOrderId
derive newtype instance encodeJsonStoredOrderId :: EncodeJson StoredOrderId
derive newtype instance decodeJsonStoredOrderId :: DecodeJson StoredOrderId

newtype StoredCustomerId = StoredCustomerId Int

derive instance genericStoredCustomerId :: Generic StoredCustomerId
derive newtype instance arbitraryStoredCustomerId :: Arbitrary StoredCustomerId
derive newtype instance eqStoredCustomerId :: Eq StoredCustomerId
derive newtype instance ordStoredCustomerId :: Ord StoredCustomerId
derive newtype instance showStoredCustomerId :: Show StoredCustomerId
derive newtype instance encodeJsonStoredCustomerId :: EncodeJson StoredCustomerId
derive newtype instance decodeJsonStoredCustomerId :: DecodeJson StoredCustomerId

newtype StoredEditorId = StoredEditorId Int

derive instance genericStoredEditorId :: Generic StoredEditorId
derive newtype instance arbitraryStoredEditorId :: Arbitrary StoredEditorId
derive newtype instance eqStoredEditorId :: Eq StoredEditorId
derive newtype instance ordStoredEditorId :: Ord StoredEditorId
derive newtype instance showStoredEditorId :: Show StoredEditorId
derive newtype instance encodeJsonStoredEditorId :: EncodeJson StoredEditorId
derive newtype instance decodeJsonStoredEditorId :: DecodeJson StoredEditorId

newtype StoredRecordSubmissionId = StoredRecordSubmissionId Int

derive instance genericStoredRecordSubmissionId :: Generic StoredRecordSubmissionId
derive newtype instance arbitraryStoredRecordSubmissionId :: Arbitrary StoredRecordSubmissionId
derive newtype instance eqStoredRecordSubmissionId :: Eq StoredRecordSubmissionId
derive newtype instance ordStoredRecordSubmissionId :: Ord StoredRecordSubmissionId
derive newtype instance showStoredRecordSubmissionId :: Show StoredRecordSubmissionId
derive newtype instance encodeJsonStoredRecordSubmissionId :: EncodeJson StoredRecordSubmissionId
derive newtype instance decodeJsonStoredRecordSubmissionId :: DecodeJson StoredRecordSubmissionId

newtype RecordSubmissionApprovalId = RecordSubmissionApprovalId Int

derive instance genericRecordSubmissionApprovalId :: Generic RecordSubmissionApprovalId
derive newtype instance arbitraryRecordSubmissionApprovalId :: Arbitrary RecordSubmissionApprovalId
derive newtype instance eqRecordSubmissionApprovalId :: Eq RecordSubmissionApprovalId
derive newtype instance ordRecordSubmissionApprovalId :: Ord RecordSubmissionApprovalId
derive newtype instance showRecordSubmissionApprovalId :: Show RecordSubmissionApprovalId
derive newtype instance encodeJsonRecordSubmissionApprovalId :: EncodeJson RecordSubmissionApprovalId
derive newtype instance decodeJsonRecordSubmissionApprovalId :: DecodeJson RecordSubmissionApprovalId

newtype StoredBlogPostId = StoredBlogPostId Int

derive instance genericStoredBlogPostId :: Generic StoredBlogPostId
derive newtype instance arbitraryStoredBlogPostId :: Arbitrary StoredBlogPostId
derive newtype instance eqStoredBlogPostId :: Eq StoredBlogPostId
derive newtype instance ordStoredBlogPostId :: Ord StoredBlogPostId
derive newtype instance showStoredBlogPostId :: Show StoredBlogPostId
derive newtype instance encodeJsonStoredBlogPostId :: EncodeJson StoredBlogPostId
derive newtype instance decodeJsonStoredBlogPostId :: DecodeJson StoredBlogPostId

newtype StoredBlogPostCategoryId = StoredBlogPostCategoryId Int

derive instance genericStoredBlogPostCategoryId :: Generic StoredBlogPostCategoryId
derive newtype instance arbitraryStoredBlogPostCategoryId :: Arbitrary StoredBlogPostCategoryId
derive newtype instance eqStoredBlogPostCategoryId :: Eq StoredBlogPostCategoryId
derive newtype instance ordStoredBlogPostCategoryId :: Ord StoredBlogPostCategoryId
derive newtype instance showStoredBlogPostCategoryId :: Show StoredBlogPostCategoryId
derive newtype instance encodeJsonStoredBlogPostCategoryId :: EncodeJson StoredBlogPostCategoryId
derive newtype instance decodeJsonStoredBlogPostCategoryId :: DecodeJson StoredBlogPostCategoryId
