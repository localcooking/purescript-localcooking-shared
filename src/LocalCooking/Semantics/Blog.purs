module LocalCooking.Semantics.Blog where

import LocalCooking.Common.User.Name (Name)
import LocalCooking.Common.Blog (BlogPostPriority, BlogPostCategory, BlogPostVariant)
import LocalCooking.Database.Schema (StoredBlogPostId, StoredBlogPostCategoryId)

import Prelude
import Data.Maybe (Maybe)
import Data.String.Markdown (MarkdownText)
import Data.String.Permalink (Permalink)
import Data.DateTime (DateTime)
import Data.DateTime.JSON (JSONDateTime (..))
import Data.Generic (class Generic, gEq, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, (:=), (~>), jsonEmptyObject, (.?))
import Test.QuickCheck (class Arbitrary, arbitrary)




-- * Category

newtype BlogPostCategorySynopsis = BlogPostCategorySynopsis
  { name      :: BlogPostCategory
  , permalink :: Permalink
  , priority  :: BlogPostPriority
  }

derive instance genericBlogPostCategorySynopsis :: Generic BlogPostCategorySynopsis

instance eqBlogPostCategorySynopsis :: Eq BlogPostCategorySynopsis where
  eq = gEq

instance showBlogPostCategorySynopsis :: Show BlogPostCategorySynopsis where
  show = gShow

instance arbitraryBlogPostCategorySynopsis :: Arbitrary BlogPostCategorySynopsis where
  arbitrary = do
    name <- arbitrary
    permalink <- arbitrary
    priority <- arbitrary
    pure (BlogPostCategorySynopsis {name,permalink,priority})

instance encodeJsonBlogPostCategorySynopsis :: EncodeJson BlogPostCategorySynopsis where
  encodeJson (BlogPostCategorySynopsis {name,permalink,priority})
    =  "name" := name
    ~> "permalink" := permalink
    ~> "priority" := priority
    ~> jsonEmptyObject

instance decodeJsonBlogPostCategorySynopsis :: DecodeJson BlogPostCategorySynopsis where
  decodeJson json = do
    o <- decodeJson json
    name <- o .? "name"
    permalink <- o .? "permalink"
    priority <- o .? "priority"
    pure (BlogPostCategorySynopsis {name,permalink,priority})

newtype GetBlogPostCategory = GetBlogPostCategory
  { name      :: BlogPostCategory
  , permalink :: Permalink
  , primary   :: Maybe BlogPostSynopsis
  , posts     :: Array BlogPostSynopsis
  , id        :: StoredBlogPostCategoryId
  }

derive instance genericGetBlogPostCategory :: Generic GetBlogPostCategory

instance eqGetBlogPostCategory :: Eq GetBlogPostCategory where
  eq = gEq

instance showGetBlogPostCategory :: Show GetBlogPostCategory where
  show = gShow

instance arbitraryGetBlogPostCategory :: Arbitrary GetBlogPostCategory where
  arbitrary = do
    name <- arbitrary
    permalink <- arbitrary
    primary <- arbitrary
    posts <- arbitrary
    id <- arbitrary
    pure (GetBlogPostCategory {name,permalink,primary,posts,id})

instance encodeJsonGetBlogPostCategory :: EncodeJson GetBlogPostCategory where
  encodeJson (GetBlogPostCategory {name,permalink,primary,posts,id})
    =  "name" := name
    ~> "permalink" := permalink
    ~> "primary" := primary
    ~> "posts" := posts
    ~> "id" := id
    ~> jsonEmptyObject

instance decodeJsonGetBlogPostCategory :: DecodeJson GetBlogPostCategory where
  decodeJson json = do
    o <- decodeJson json
    name <- o .? "name"
    permalink <- o .? "permalink"
    primary <- o .? "primary"
    posts <- o .? "posts"
    id <- o .? "id"
    pure (GetBlogPostCategory {name,permalink,primary,posts,id})
    
newtype NewBlogPostCategory = NewBlogPostCategory
  { name      :: BlogPostCategory
  , permalink :: Permalink
  , priority  :: BlogPostPriority
  }

derive instance genericNewBlogPostCategory :: Generic NewBlogPostCategory

instance eqNewBlogPostCategory :: Eq NewBlogPostCategory where
  eq = gEq

instance showNewBlogPostCategory :: Show NewBlogPostCategory where
  show = gShow

instance arbitraryNewBlogPostCategory :: Arbitrary NewBlogPostCategory where
  arbitrary = do
    name <- arbitrary
    permalink <- arbitrary
    priority <- arbitrary
    pure (NewBlogPostCategory {name,permalink,priority})

instance encodeJsonNewBlogPostCategory :: EncodeJson NewBlogPostCategory where
  encodeJson (NewBlogPostCategory {name,permalink,priority})
    =  "name" := name
    ~> "permalink" := permalink
    ~> "priority" := priority
    ~> jsonEmptyObject

instance decodeJsonNewBlogPostCategory :: DecodeJson NewBlogPostCategory where
  decodeJson json = do
    o <- decodeJson json
    name <- o .? "name"
    permalink <- o .? "permalink"
    priority <- o .? "priority"
    pure (NewBlogPostCategory {name,permalink,priority})
    
newtype SetBlogPostCategory = SetBlogPostCategory
  { name      :: BlogPostCategory
  , permalink :: Permalink
  , priority  :: BlogPostPriority
  , id        :: StoredBlogPostCategoryId
  }

derive instance genericSetBlogPostCategory :: Generic SetBlogPostCategory

instance eqSetBlogPostCategory :: Eq SetBlogPostCategory where
  eq = gEq

instance showSetBlogPostCategory :: Show SetBlogPostCategory where
  show = gShow

instance arbitrarySetBlogPostCategory :: Arbitrary SetBlogPostCategory where
  arbitrary = do
    name <- arbitrary
    permalink <- arbitrary
    priority <- arbitrary
    id <- arbitrary
    pure (SetBlogPostCategory {name,permalink,priority,id})

instance encodeJsonSetBlogPostCategory :: EncodeJson SetBlogPostCategory where
  encodeJson (SetBlogPostCategory {name,permalink,priority,id})
    =  "name" := name
    ~> "permalink" := permalink
    ~> "priority" := priority
    ~> "id" := id
    ~> jsonEmptyObject

instance decodeJsonSetBlogPostCategory :: DecodeJson SetBlogPostCategory where
  decodeJson json = do
    o <- decodeJson json
    name <- o .? "name"
    permalink <- o .? "permalink"
    priority <- o .? "priority"
    id <- o .? "id"
    pure (SetBlogPostCategory {name,permalink,priority,id})


-- * Post

newtype BlogPostSynopsis = BlogPostSynopsis
  { author    :: Name
  , timestamp :: DateTime
  , headline  :: String
  , permalink :: Permalink
  , variant   :: BlogPostVariant
  , priority  :: BlogPostPriority
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
    variant <- arbitrary
    priority <- arbitrary
    pure (BlogPostSynopsis {author,timestamp,headline,permalink,variant,priority})

instance encodeJsonBlogPostSynopsis :: EncodeJson BlogPostSynopsis where
  encodeJson (BlogPostSynopsis {author,timestamp,headline,permalink,variant,priority})
    =  "author" := author
    ~> "timestamp" := JSONDateTime timestamp
    ~> "headline" := headline
    ~> "permalink" := permalink
    ~> "variant" := variant
    ~> "priority" := priority
    ~> jsonEmptyObject

instance decodeJsonBlogPostSynopsis :: DecodeJson BlogPostSynopsis where
  decodeJson json = do
    o <- decodeJson json
    author <- o .? "author"
    JSONDateTime timestamp <- o .? "timestamp"
    headline <- o .? "headline"
    permalink <- o .? "permalink"
    variant <- o .? "variant"
    priority <- o .? "priority"
    pure (BlogPostSynopsis {author,timestamp,headline,permalink,variant,priority})


newtype GetBlogPost = GetBlogPost
  { author    :: Name
  , timestamp :: DateTime
  , headline  :: String
  , permalink :: Permalink
  , content   :: MarkdownText
  , variant   :: BlogPostVariant
  , priority  :: BlogPostPriority
  , category  :: BlogPostCategory
  , id        :: StoredBlogPostId
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
    variant <- arbitrary
    priority <- arbitrary
    category <- arbitrary
    id <- arbitrary
    pure (GetBlogPost {author,timestamp,headline,permalink,content,variant,priority,category,id})

instance encodeJsonGetBlogPost :: EncodeJson GetBlogPost where
  encodeJson (GetBlogPost {author,timestamp,headline,permalink,content,variant,priority,category,id})
    =  "author" := author
    ~> "timestamp" := JSONDateTime timestamp
    ~> "headline" := headline
    ~> "permalink" := permalink
    ~> "content" := content
    ~> "variant" := variant
    ~> "priority" := priority
    ~> "category" := category
    ~> "id" := id
    ~> jsonEmptyObject

instance decodeJsonGetBlogPost :: DecodeJson GetBlogPost where
  decodeJson json = do
    o <- decodeJson json
    author <- o .? "author"
    JSONDateTime timestamp <- o .? "timestamp"
    headline <- o .? "headline"
    permalink <- o .? "permalink"
    content <- o .? "content"
    variant <- o .? "variant"
    priority <- o .? "priority"
    category <- o .? "category"
    id <- o .? "id"
    pure (GetBlogPost {author,timestamp,headline,permalink,content,variant,priority,category,id})


newtype NewBlogPost = NewBlogPost
  { headline  :: String
  , permalink :: Permalink
  , content   :: MarkdownText
  , variant   :: BlogPostVariant
  , priority  :: BlogPostPriority
  , category  :: BlogPostCategory
  , id        :: StoredBlogPostId
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
    variant <- arbitrary
    priority <- arbitrary
    category <- arbitrary
    id <- arbitrary
    pure (NewBlogPost {headline,permalink,content,variant,priority,category,id})

instance encodeJsonNewBlogPost :: EncodeJson NewBlogPost where
  encodeJson (NewBlogPost {headline,permalink,content,variant,priority,category,id})
    =  "headline" := headline
    ~> "permalink" := permalink
    ~> "content" := content
    ~> "variant" := variant
    ~> "priority" := priority
    ~> "category" := category
    ~> "id" := id
    ~> jsonEmptyObject

instance decodeJsonNewBlogPost :: DecodeJson NewBlogPost where
  decodeJson json = do
    o <- decodeJson json
    headline <- o .? "headline"
    permalink <- o .? "permalink"
    content <- o .? "content"
    variant <- o .? "variant"
    priority <- o .? "priority"
    category <- o .? "category"
    id <- o .? "id"
    pure (NewBlogPost {headline,permalink,content,variant,priority,category,id})


newtype SetBlogPost = SetBlogPost
  { headline :: String
  , permalink :: Permalink
  , content   :: MarkdownText
  , variant   :: BlogPostVariant
  , priority  :: BlogPostPriority
  , category  :: BlogPostCategory
  , id        :: StoredBlogPostId
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
    variant <- arbitrary
    priority <- arbitrary
    category <- arbitrary
    id <- arbitrary
    pure (SetBlogPost {headline,permalink,content,variant,priority,category,id})

instance encodeJsonSetBlogPost :: EncodeJson SetBlogPost where
  encodeJson (SetBlogPost {headline,permalink,content,variant,priority,category,id})
    =  "headline" := headline
    ~> "permalink" := permalink
    ~> "content" := content
    ~> "variant" := variant
    ~> "priority" := priority
    ~> "category" := category
    ~> "id" := id
    ~> jsonEmptyObject

instance decodeJsonSetBlogPost :: DecodeJson SetBlogPost where
  decodeJson json = do
    o <- decodeJson json
    headline <- o .? "headline"
    permalink <- o .? "permalink"
    content <- o .? "content"
    variant <- o .? "variant"
    priority <- o .? "priority"
    category <- o .? "category"
    id <- o .? "id"
    pure (SetBlogPost {headline,permalink,content,variant,priority,category,id})
