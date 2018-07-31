module LocalCooking.Semantics.Blog where

import LocalCooking.Common.Blog (BlogPostPriority, BlogPostCategory, BlogPostVariant)
import LocalCooking.Database.Schema (StoredBlogPostId, StoredBlogPostCategoryId)

import Prelude
import Data.Name (Name)
import Data.Maybe (Maybe)
import Data.String.Markdown (MarkdownText)
import Data.String.Permalink (Permalink)
import Data.DateTime (DateTime)
import Data.Generic (class Generic, gEq, gShow)
import Data.NonEmpty (NonEmpty (..))
import Data.Argonaut
  ( class EncodeJson, class DecodeJson, encodeJson, decodeJson
  , fail, (:=), (~>), jsonEmptyObject, (.?))
import Data.Argonaut.JSONDateTime (JSONDateTime (..))
import Control.Alternative ((<|>))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)




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
  , variant   :: BlogPostVariant
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
    variant <- arbitrary
    id <- arbitrary
    pure (GetBlogPostCategory {name,permalink,primary,posts,variant,id})

instance encodeJsonGetBlogPostCategory :: EncodeJson GetBlogPostCategory where
  encodeJson (GetBlogPostCategory {name,permalink,primary,posts,variant,id})
    =  "name" := name
    ~> "permalink" := permalink
    ~> "primary" := primary
    ~> "posts" := posts
    ~> "variant" := variant
    ~> "id" := id
    ~> jsonEmptyObject

instance decodeJsonGetBlogPostCategory :: DecodeJson GetBlogPostCategory where
  decodeJson json = do
    o <- decodeJson json
    name <- o .? "name"
    permalink <- o .? "permalink"
    primary <- o .? "primary"
    posts <- o .? "posts"
    variant <- o .? "variant"
    id <- o .? "id"
    pure (GetBlogPostCategory {name,permalink,primary,posts,variant,id})
    
newtype NewBlogPostCategory = NewBlogPostCategory
  { name      :: BlogPostCategory
  , permalink :: Permalink
  , priority  :: BlogPostPriority
  , variant   :: BlogPostVariant
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
    variant <- arbitrary
    pure (NewBlogPostCategory {name,permalink,priority,variant})

instance encodeJsonNewBlogPostCategory :: EncodeJson NewBlogPostCategory where
  encodeJson (NewBlogPostCategory {name,permalink,priority,variant})
    =  "name" := name
    ~> "permalink" := permalink
    ~> "priority" := priority
    ~> "variant" := variant
    ~> jsonEmptyObject

instance decodeJsonNewBlogPostCategory :: DecodeJson NewBlogPostCategory where
  decodeJson json = do
    o <- decodeJson json
    name <- o .? "name"
    permalink <- o .? "permalink"
    priority <- o .? "priority"
    variant <- o .? "variant"
    pure (NewBlogPostCategory {name,permalink,priority,variant})
    
newtype SetBlogPostCategory = SetBlogPostCategory
  { name      :: BlogPostCategory
  , permalink :: Permalink
  , priority  :: BlogPostPriority
  , variant   :: BlogPostVariant
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
    variant <- arbitrary
    id <- arbitrary
    pure (SetBlogPostCategory {name,permalink,priority,variant,id})

instance encodeJsonSetBlogPostCategory :: EncodeJson SetBlogPostCategory where
  encodeJson (SetBlogPostCategory {name,permalink,priority,variant,id})
    =  "name" := name
    ~> "permalink" := permalink
    ~> "priority" := priority
    ~> "variant" := variant
    ~> "id" := id
    ~> jsonEmptyObject

instance decodeJsonSetBlogPostCategory :: DecodeJson SetBlogPostCategory where
  decodeJson json = do
    o <- decodeJson json
    name <- o .? "name"
    permalink <- o .? "permalink"
    priority <- o .? "priority"
    variant <- o .? "variant"
    id <- o .? "id"
    pure (SetBlogPostCategory {name,permalink,priority,variant,id})


-- * Post

newtype BlogPostSynopsis = BlogPostSynopsis
  { author    :: Name
  , timestamp :: DateTime
  , headline  :: String
  , permalink :: Permalink
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
    priority <- arbitrary
    pure (BlogPostSynopsis {author,timestamp,headline,permalink,priority})

instance encodeJsonBlogPostSynopsis :: EncodeJson BlogPostSynopsis where
  encodeJson (BlogPostSynopsis {author,timestamp,headline,permalink,priority})
    =  "author" := author
    ~> "timestamp" := JSONDateTime timestamp
    ~> "headline" := headline
    ~> "permalink" := permalink
    ~> "priority" := priority
    ~> jsonEmptyObject

instance decodeJsonBlogPostSynopsis :: DecodeJson BlogPostSynopsis where
  decodeJson json = do
    o <- decodeJson json
    author <- o .? "author"
    JSONDateTime timestamp <- o .? "timestamp"
    headline <- o .? "headline"
    permalink <- o .? "permalink"
    priority <- o .? "priority"
    pure (BlogPostSynopsis {author,timestamp,headline,permalink,priority})


newtype GetBlogPost = GetBlogPost
  { author    :: Name
  , timestamp :: DateTime
  , headline  :: String
  , permalink :: Permalink
  , content   :: MarkdownText
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
    priority <- arbitrary
    category <- arbitrary
    id <- arbitrary
    pure (GetBlogPost {author,timestamp,headline,permalink,content,priority,category,id})

instance encodeJsonGetBlogPost :: EncodeJson GetBlogPost where
  encodeJson (GetBlogPost {author,timestamp,headline,permalink,content,priority,category,id})
    =  "author" := author
    ~> "timestamp" := JSONDateTime timestamp
    ~> "headline" := headline
    ~> "permalink" := permalink
    ~> "content" := content
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
    priority <- o .? "priority"
    category <- o .? "category"
    id <- o .? "id"
    pure (GetBlogPost {author,timestamp,headline,permalink,content,priority,category,id})


newtype NewBlogPost = NewBlogPost
  { headline  :: String
  , permalink :: Permalink
  , content   :: MarkdownText
  , priority  :: BlogPostPriority
  , category  :: StoredBlogPostCategoryId
  , primary   :: Boolean
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
    priority <- arbitrary
    category <- arbitrary
    primary <- arbitrary
    pure (NewBlogPost {headline,permalink,content,priority,category,primary})

instance encodeJsonNewBlogPost :: EncodeJson NewBlogPost where
  encodeJson (NewBlogPost {headline,permalink,content,priority,category,primary})
    =  "headline" := headline
    ~> "permalink" := permalink
    ~> "content" := content
    ~> "priority" := priority
    ~> "category" := category
    ~> "primary" := primary
    ~> jsonEmptyObject

instance decodeJsonNewBlogPost :: DecodeJson NewBlogPost where
  decodeJson json = do
    o <- decodeJson json
    headline <- o .? "headline"
    permalink <- o .? "permalink"
    content <- o .? "content"
    priority <- o .? "priority"
    category <- o .? "category"
    primary <- o .? "primary"
    pure (NewBlogPost {headline,permalink,content,priority,category,primary})


newtype SetBlogPost = SetBlogPost
  { headline  :: String
  , permalink :: Permalink
  , content   :: MarkdownText
  , priority  :: BlogPostPriority
  , primary   :: Boolean
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
    priority <- arbitrary
    primary <- arbitrary
    id <- arbitrary
    pure (SetBlogPost {headline,permalink,content,priority,primary,id})

instance encodeJsonSetBlogPost :: EncodeJson SetBlogPost where
  encodeJson (SetBlogPost {headline,permalink,content,priority,primary,id})
    =  "headline" := headline
    ~> "permalink" := permalink
    ~> "content" := content
    ~> "priority" := priority
    ~> "primary" := primary
    ~> "id" := id
    ~> jsonEmptyObject

instance decodeJsonSetBlogPost :: DecodeJson SetBlogPost where
  decodeJson json = do
    o <- decodeJson json
    headline <- o .? "headline"
    permalink <- o .? "permalink"
    content <- o .? "content"
    priority <- o .? "priority"
    primary <- o .? "primary"
    id <- o .? "id"
    pure (SetBlogPost {headline,permalink,content,priority,primary,id})


-- * Errors



data BlogPostCategoryExists a
  = BlogPostCategoryDoesntExist
  | BlogPostCategoryExists a

derive instance genericBlogPostCategoryExists :: Generic a => Generic (BlogPostCategoryExists a)

instance arbitraryBlogPostCategoryExists :: Arbitrary a => Arbitrary (BlogPostCategoryExists a) where
  arbitrary = oneOf $ NonEmpty
    ( pure BlogPostCategoryDoesntExist
    )
    [ BlogPostCategoryExists <$> arbitrary
    ]

instance eqBlogPostCategoryExists :: Generic a => Eq (BlogPostCategoryExists a) where
  eq = gEq

instance showBlogPostCategoryExists :: Generic a => Show (BlogPostCategoryExists a) where
  show = gShow

instance encodeJsonBlogPostCategoryExists :: EncodeJson a => EncodeJson (BlogPostCategoryExists a) where
  encodeJson x = case x of
    BlogPostCategoryDoesntExist -> encodeJson "blogPostCategoryDoesntExist"
    BlogPostCategoryExists y
      -> "blogPostCategoryExists"
      := y
      ~> jsonEmptyObject

instance decodeJsonBlogPostCategoryExists :: DecodeJson a => DecodeJson (BlogPostCategoryExists a) where
  decodeJson json = do
    let empty = do
          s <- decodeJson json
          if s == "blogPostCategoryDoesntExist"
             then pure BlogPostCategoryDoesntExist
             else fail "Not a BlogPostCategoryExists"
        has = do
          o <- decodeJson json
          BlogPostCategoryExists <$> o .? "blogPostCategoryExists"
    empty <|> has


data BlogPostCategoryUnique a
  = BlogPostCategoryNotUnique
  | BlogPostCategoryUnique a

derive instance genericBlogPostCategoryUnique :: Generic a => Generic (BlogPostCategoryUnique a)

instance arbitraryBlogPostCategoryUnique :: Arbitrary a => Arbitrary (BlogPostCategoryUnique a) where
  arbitrary = oneOf $ NonEmpty
    ( pure BlogPostCategoryNotUnique
    )
    [ BlogPostCategoryUnique <$> arbitrary
    ]

instance eqBlogPostCategoryUnique :: Generic a => Eq (BlogPostCategoryUnique a) where
  eq = gEq

instance showBlogPostCategoryUnique :: Generic a => Show (BlogPostCategoryUnique a) where
  show = gShow

instance encodeJsonBlogPostCategoryUnique :: EncodeJson a => EncodeJson (BlogPostCategoryUnique a) where
  encodeJson x = case x of
    BlogPostCategoryNotUnique -> encodeJson "blogPostCategoryNotUnique"
    BlogPostCategoryUnique y
      -> "blogPostCategoryUnique"
      := y
      ~> jsonEmptyObject

instance decodeJsonBlogPostCategoryUnique :: DecodeJson a => DecodeJson (BlogPostCategoryUnique a) where
  decodeJson json = do
    let empty = do
          s <- decodeJson json
          if s == "blogPostCategoryNotUnique"
             then pure BlogPostCategoryNotUnique
             else fail "Not a BlogPostCategoryUnique"
        has = do
          o <- decodeJson json
          BlogPostCategoryUnique <$> o .? "blogPostCategoryUnique"
    empty <|> has



data BlogPostExists a
  = BlogPostDoesntExist
  | BlogPostExists a

derive instance genericBlogPostExists :: Generic a => Generic (BlogPostExists a)

instance arbitraryBlogPostExists :: Arbitrary a => Arbitrary (BlogPostExists a) where
  arbitrary = oneOf $ NonEmpty
    ( pure BlogPostDoesntExist
    )
    [ BlogPostExists <$> arbitrary
    ]

instance eqBlogPostExists :: Generic a => Eq (BlogPostExists a) where
  eq = gEq

instance showBlogPostExists :: Generic a => Show (BlogPostExists a) where
  show = gShow

instance encodeJsonBlogPostExists :: EncodeJson a => EncodeJson (BlogPostExists a) where
  encodeJson x = case x of
    BlogPostDoesntExist -> encodeJson "blogPostDoesntExist"
    BlogPostExists y
      -> "blogPostExists"
      := y
      ~> jsonEmptyObject

instance decodeJsonBlogPostExists :: DecodeJson a => DecodeJson (BlogPostExists a) where
  decodeJson json = do
    let empty = do
          s <- decodeJson json
          if s == "blogPostDoesntExist"
             then pure BlogPostDoesntExist
             else fail "Not a BlogPostExists"
        has = do
          o <- decodeJson json
          BlogPostExists <$> o .? "blogPostExists"
    empty <|> has


data BlogPostUnique a
  = BlogPostNotUnique
  | BlogPostUnique a

derive instance genericBlogPostUnique :: Generic a => Generic (BlogPostUnique a)

instance arbitraryBlogPostUnique :: Arbitrary a => Arbitrary (BlogPostUnique a) where
  arbitrary = oneOf $ NonEmpty
    ( pure BlogPostNotUnique
    )
    [ BlogPostUnique <$> arbitrary
    ]

instance eqBlogPostUnique :: Generic a => Eq (BlogPostUnique a) where
  eq = gEq

instance showBlogPostUnique :: Generic a => Show (BlogPostUnique a) where
  show = gShow

instance encodeJsonBlogPostUnique :: EncodeJson a => EncodeJson (BlogPostUnique a) where
  encodeJson x = case x of
    BlogPostNotUnique -> encodeJson "blogPostNotUnique"
    BlogPostUnique y
      -> "blogPostUnique"
      := y
      ~> jsonEmptyObject

instance decodeJsonBlogPostUnique :: DecodeJson a => DecodeJson (BlogPostUnique a) where
  decodeJson json = do
    let empty = do
          s <- decodeJson json
          if s == "blogPostNotUnique"
             then pure BlogPostNotUnique
             else fail "Not a BlogPostUnique"
        has = do
          o <- decodeJson json
          BlogPostUnique <$> o .? "blogPostUnique"
    empty <|> has


data BlogPostPrimary a
  = BlogPostNotPrimary
  | BlogPostPrimary a

derive instance genericBlogPostPrimary :: Generic a => Generic (BlogPostPrimary a)

instance arbitraryBlogPostPrimary :: Arbitrary a => Arbitrary (BlogPostPrimary a) where
  arbitrary = oneOf $ NonEmpty
    ( pure BlogPostNotPrimary
    )
    [ BlogPostPrimary <$> arbitrary
    ]

instance eqBlogPostPrimary :: Generic a => Eq (BlogPostPrimary a) where
  eq = gEq

instance showBlogPostPrimary :: Generic a => Show (BlogPostPrimary a) where
  show = gShow

instance encodeJsonBlogPostPrimary :: EncodeJson a => EncodeJson (BlogPostPrimary a) where
  encodeJson x = case x of
    BlogPostNotPrimary -> encodeJson "blogPostNotPrimary"
    BlogPostPrimary y
      -> "blogPostPrimary"
      := y
      ~> jsonEmptyObject

instance decodeJsonBlogPostPrimary :: DecodeJson a => DecodeJson (BlogPostPrimary a) where
  decodeJson json = do
    let empty = do
          s <- decodeJson json
          if s == "blogPostNotPrimary"
             then pure BlogPostNotPrimary
             else fail "Not a BlogPostPrimary"
        has = do
          o <- decodeJson json
          BlogPostPrimary <$> o .? "blogPostPrimary"
    empty <|> has
