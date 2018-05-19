module Data.Date.JSON where

import Prelude
import Data.Int as Int
import Data.JSDate as JSDate
import Data.Date (Date)
import Data.Date as Date
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Enum (fromEnum)
import Data.String as String
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, fail)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Exception (try)


newtype JSONDate = JSONDate Date

getJSONDate :: JSONDate -> Date
getJSONDate (JSONDate x) = x

instance encodeJsonJSONDate :: EncodeJson JSONDate where
  encodeJson (JSONDate x) =
    let date' = JSDate.jsdate
          { year: Int.toNumber $ fromEnum $ Date.year x
          , month: Int.toNumber $ fromEnum $ Date.month x
          , day: Int.toNumber $ fromEnum $ Date.day x
          , hour: 0.0
          , minute: 0.0
          , second: 0.0
          , millisecond: 0.0
          }
    in  encodeJson $ String.take 10 $ unsafePerformEff $ JSDate.toISOString date'

instance decodeJsonJSONDate :: DecodeJson JSONDate where
  decodeJson json = do
    s <- decodeJson json
    case unsafePerformEff $ try $ JSDate.parse s of
      Left _ -> fail "Not a date"
      Right x -> case JSDate.toDate x of
        Nothing -> fail "Not a date"
        Just y -> pure (JSONDate y)
