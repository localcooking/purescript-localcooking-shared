module LocalCooking.Semantics.Blog where

import LocalCooking.Common.User.Name (Name)

import Prelude
import Data.String.Markdown (MarkdownText)
import Data.String.Permalink (Permalink)
import Data.DateTime (DateTime)
import Data.DateTime.JSON (JSONDateTime (..))
import Data.Generic (class Generic, gEq, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, (:=), (~>), jsonEmptyObject, (.?))
import Test.QuickCheck (class Arbitrary, arbitrary)



newtype BlogPostSynopsis = BlogPostSynopsis
  { author :: Name
  , timestamp :: DateTime
  , headline :: String
  , permalink :: Permalink
  }

derive instance genericBlogPostSynopsis :: Generic BlogPostSynopsis

instance eqBlogPostSynopsis :: Eq BlogPostSynopsis where
  eq = gEq

instance showBlogPostSynopsis :: Show BlogPostSynopsis where
  show = gShow

instance arbitraryBlogPostSynopsis :: Arbitrary BlogPostSynopsis where
  arbitrary = do
    author <- arbitrary
    JSONDateTime timestamp <- arbitrary
    headline <- arbitrary
    permalink <- arbitrary
    pure (BlogPostSynopsis {author,timestamp,headline,permalink})

instance encodeJsonBlogPostSynopsis :: EncodeJson BlogPostSynopsis where
  encodeJson (BlogPostSynopsis {author,timestamp,headline,permalink})
    =  "author" := author
    ~> "timestamp" := JSONDateTime timestamp
    ~> "headline" := headline
    ~> "permalink" := permalink
    ~> jsonEmptyObject

instance decodeJsonBlogPostSynopsis :: DecodeJson BlogPostSynopsis where
  decodeJson json = do
    o <- decodeJson json
    author <- o .? "author"
    JSONDateTime timestamp <- o .? "timestamp"
    headline <- o .? "headline"
    permalink <- o .? "permalink"
    pure (BlogPostSynopsis {author,timestamp,headline,permalink})


newtype GetBlogPost = GetBlogPost
  { author :: Name
  , timestamp :: DateTime
  , headline :: String
  , permalink :: Permalink
  , content   :: MarkdownText
  }

derive instance genericGetBlogPost :: Generic GetBlogPost

instance eqGetBlogPost :: Eq GetBlogPost where
  eq = gEq

instance showGetBlogPost :: Show GetBlogPost where
  show = gShow

instance arbitraryGetBlogPost :: Arbitrary GetBlogPost where
  arbitrary = do
    author <- arbitrary
    JSONDateTime timestamp <- arbitrary
    headline <- arbitrary
    permalink <- arbitrary
    content <- arbitrary
    pure (GetBlogPost {author,timestamp,headline,permalink,content})

instance encodeJsonGetBlogPost :: EncodeJson GetBlogPost where
  encodeJson (GetBlogPost {author,timestamp,headline,permalink,content})
    =  "author" := author
    ~> "timestamp" := JSONDateTime timestamp
    ~> "headline" := headline
    ~> "permalink" := permalink
    ~> "content" := content
    ~> jsonEmptyObject

instance decodeJsonGetBlogPost :: DecodeJson GetBlogPost where
  decodeJson json = do
    o <- decodeJson json
    author <- o .? "author"
    JSONDateTime timestamp <- o .? "timestamp"
    headline <- o .? "headline"
    permalink <- o .? "permalink"
    content <- o .? "content"
    pure (GetBlogPost {author,timestamp,headline,permalink,content})


newtype NewBlogPost = NewBlogPost
  { headline :: String
  , permalink :: Permalink
  , content   :: MarkdownText
  }

derive instance genericNewBlogPost :: Generic NewBlogPost

instance eqNewBlogPost :: Eq NewBlogPost where
  eq = gEq

instance showNewBlogPost :: Show NewBlogPost where
  show = gShow

instance arbitraryNewBlogPost :: Arbitrary NewBlogPost where
  arbitrary = do
    headline <- arbitrary
    permalink <- arbitrary
    content <- arbitrary
    pure (NewBlogPost {headline,permalink,content})

instance encodeJsonNewBlogPost :: EncodeJson NewBlogPost where
  encodeJson (NewBlogPost {headline,permalink,content})
    =  "headline" := headline
    ~> "permalink" := permalink
    ~> "content" := content
    ~> jsonEmptyObject

instance decodeJsonNewBlogPost :: DecodeJson NewBlogPost where
  decodeJson json = do
    o <- decodeJson json
    headline <- o .? "headline"
    permalink <- o .? "permalink"
    content <- o .? "content"
    pure (NewBlogPost {headline,permalink,content})


newtype SetBlogPost = SetBlogPost
  { headline :: String
  , permalink :: Permalink
  , content   :: MarkdownText
  }

derive instance genericSetBlogPost :: Generic SetBlogPost

instance eqSetBlogPost :: Eq SetBlogPost where
  eq = gEq

instance showSetBlogPost :: Show SetBlogPost where
  show = gShow

instance arbitrarySetBlogPost :: Arbitrary SetBlogPost where
  arbitrary = do
    headline <- arbitrary
    permalink <- arbitrary
    content <- arbitrary
    pure (SetBlogPost {headline,permalink,content})

instance encodeJsonSetBlogPost :: EncodeJson SetBlogPost where
  encodeJson (SetBlogPost {headline,permalink,content})
    =  "headline" := headline
    ~> "permalink" := permalink
    ~> "content" := content
    ~> jsonEmptyObject

instance decodeJsonSetBlogPost :: DecodeJson SetBlogPost where
  decodeJson json = do
    o <- decodeJson json
    headline <- o .? "headline"
    permalink <- o .? "permalink"
    content <- o .? "content"
    pure (SetBlogPost {headline,permalink,content})
