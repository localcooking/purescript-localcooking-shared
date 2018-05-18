module LocalCooking.Semantic.Mitch.Review where

import LocalCooking.Common.Rating (Rating)

import Prelude
import Data.String.Markdown (MarkdownText)
import Data.Image.Source (ImageSource)
import Data.Generic (class Generic, gShow, gEq)
import Data.Argonaut (class EncodeJson, class DecodeJson, (~>), (:=), decodeJson, (.?), jsonEmptyObject)


newtype ReviewSynopsis = ReviewSynopsis
  { rating  :: Rating
  , heading :: String
  }

derive instance genericReviewSynopsis :: Generic ReviewSynopsis

instance eqReviewSynopsis :: Eq ReviewSynopsis where
  eq = gEq

instance showReviewSynopsis :: Show ReviewSynopsis where
  show = gShow

instance encodeJsonReviewSynopsis :: EncodeJson ReviewSynopsis where
  encodeJson (ReviewSynopsis {rating,heading})
    =  "rating" := rating
    ~> "heading" := heading
    ~> jsonEmptyObject

instance decodeJsonReviewSynopsis :: DecodeJson ReviewSynopsis where
  decodeJson json = do
    o <- decodeJson json
    rating <- o .? "rating"
    heading <- o .? "heading"
    pure (ReviewSynopsis {rating,heading})


newtype Review = Review
  { rating  :: Rating
  , heading :: String
  , body    :: MarkdownText
  , images  :: Array ImageSource
  }


derive instance genericReview :: Generic Review

instance eqReview :: Eq Review where
  eq = gEq

instance showReview :: Show Review where
  show = gShow

instance encodeJsonReview :: EncodeJson Review where
  encodeJson (Review {rating,heading,body,images})
    =  "rating" := rating
    ~> "heading" := heading
    ~> "body" := body
    ~> "images" := images
    ~> jsonEmptyObject

instance decodeJsonReview :: DecodeJson Review where
  decodeJson json = do
    o <- decodeJson json
    rating <- o .? "rating"
    heading <- o .? "heading"
    body <- o .? "body"
    images <- o .? "images"
    pure (Review {rating,heading,body,images})
