module LocalCooking.Semantic.Mitch.Menu where

import LocalCooking.Semantic.Mitch.Chef (ChefSynopsis)
import LocalCooking.Semantic.Mitch.Meal (MealSynopsis)

import Prelude
import Data.Int as Int
import Data.JSDate as JSDate
import Data.Date (Date)
import Data.Date as Date
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Enum (fromEnum)
import Data.String as String
import Data.String.Markdown (MarkdownText)
import Data.Generic (class Generic, gShow, gEq)
import Data.Argonaut (class EncodeJson, class DecodeJson, (~>), (:=), decodeJson, (.?), jsonEmptyObject, fail)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Exception (try)



newtype Menu = Menu
  { published   :: Maybe Date
  , deadline    :: Date
  , description :: MarkdownText
  , author      :: ChefSynopsis
  , meals       :: Array MealSynopsis
  }

derive instance genericMenu :: Generic Menu

instance eqMenu :: Eq Menu where
  eq = gEq

instance showMenu :: Show Menu where
  show = gShow

instance encodeJsonMenu :: EncodeJson Menu where
  encodeJson (Menu {published,deadline,description,author,meals})
    =  "published" := (dateToJson <$> published)
    ~> "deadline" := dateToJson deadline
    ~> "description" := description
    ~> "author" := author
    ~> "meals" := meals
    ~> jsonEmptyObject
    where
      dateToJson x =
        let date' = JSDate.jsdate
              { year: Int.toNumber $ fromEnum $ Date.year x
              , month: Int.toNumber $ fromEnum $ Date.month x
              , day: Int.toNumber $ fromEnum $ Date.day x
              , hour: 0.0
              , minute: 0.0
              , second: 0.0
              , millisecond: 0.0
              }
        in  String.take 10 $ unsafePerformEff $ JSDate.toISOString date'

instance decodeJsonMenu :: DecodeJson Menu where
  decodeJson json = do
    o <- decodeJson json
    published <- optionally decodeDate =<< o .? "published"
    deadline <- decodeDate =<< o .? "deadline"
    description <- o .? "description"
    author <- o .? "author"
    meals <- o .? "meals"
    pure (Menu {published,deadline,description,author,meals})
    where
      optionally :: forall a json
                  . (json -> Either String a)
                 -> Maybe json -> Either String (Maybe a)
      optionally p mx = case mx of
        Nothing -> pure Nothing
        Just x -> Just <$> p x
      decodeDate :: String -> Either String Date
      decodeDate s = case unsafePerformEff $ try $ JSDate.parse s of
        Left _ -> fail "Not a date"
        Right x -> case JSDate.toDate x of
          Nothing -> fail "Not a date"
          Just y -> pure y
