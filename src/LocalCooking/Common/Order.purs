module LocalCooking.Common.Order where

import Prelude
import Data.NonEmpty (NonEmpty (..))
import Data.Generic (class Generic, gEq, gCompare, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, fail)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (oneOf)


data OrderProgress
  = PrepProgress
  | Cooking1Progress
  | Cooking2Progress
  | CookedProgress
  | FreezingProgress
  | PackagingProgress
  | DeliveringProgress
  | DeliveredProgress

derive instance genericOrderProgress :: Generic OrderProgress

instance eqOrderProgress :: Eq OrderProgress where
  eq = gEq

instance ordRating :: Ord OrderProgress where
  compare = gCompare

instance showOrderProgress :: Show OrderProgress where
  show = gShow

instance arbitraryOrderProgress :: Arbitrary OrderProgress where
  arbitrary = oneOf $ NonEmpty
    ( pure PrepProgress
    )
    [ pure Cooking1Progress
    , pure Cooking2Progress
    , pure CookedProgress
    , pure FreezingProgress
    , pure PackagingProgress
    , pure DeliveringProgress
    , pure DeliveredProgress
    ]

instance encodeJsonOrderProgress :: EncodeJson OrderProgress where
  encodeJson x = encodeJson $ case x of
    PrepProgress -> "prep"
    Cooking1Progress -> "cook1"
    Cooking2Progress -> "cook2"
    CookedProgress -> "cooked"
    FreezingProgress -> "freezing"
    PackagingProgress -> "pack"
    DeliveringProgress -> "deliv"
    DeliveredProgress -> "done"

instance decodeJsonOrderProgress :: DecodeJson OrderProgress where
  decodeJson json = do
    s <- decodeJson json
    case unit of
      _ | s == "prep" -> pure PrepProgress
        | s == "cook1" -> pure Cooking1Progress
        | s == "cook2" -> pure Cooking2Progress
        | s == "cooked" -> pure CookedProgress
        | s == "freezing" -> pure FreezingProgress
        | s == "pack" -> pure PackagingProgress
        | s == "deliv" -> pure DeliveringProgress
        | s == "done" -> pure DeliveredProgress
        | otherwise -> fail "OrderProgress"
