module LocalCooking.Common.Rating where

import Prelude
import Data.Generic (class Generic, gEq, gCompare)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, fail)


data Rating
  = ZeroStar
  | HalfStar
  | OneStar
  | OneHalfStar
  | TwoStar
  | TwoHalfStar
  | ThreeStar
  | ThreeHalfStar
  | FourStar
  | FourHalfStar
  | FiveStar

derive instance genericRating :: Generic Rating

instance eqRating :: Eq Rating where
  eq = gEq

instance ordRating :: Ord Rating where
  compare = gCompare

instance showRating :: Show Rating where
  show x = case x of
    ZeroStar -> "0"
    HalfStar -> "1/2"
    OneStar -> "1"
    OneHalfStar -> "3/2"
    TwoStar -> "2"
    TwoHalfStar -> "5/2"
    ThreeStar -> "3"
    ThreeHalfStar -> "7/2"
    FourStar -> "4"
    FourHalfStar -> "9/2"
    FiveStar -> "5"

instance encodeJsonRating :: EncodeJson Rating where
  encodeJson = encodeJson <<< show

instance decodeJsonRating :: DecodeJson Rating where
  decodeJson json = do
    s <- decodeJson json
    case unit of
      _ | s == "0" -> pure ZeroStar
        | s == "1/2" -> pure HalfStar
        | s == "1" -> pure OneStar
        | s == "3/2" -> pure OneHalfStar
        | s == "2" -> pure TwoStar
        | s == "5/2" -> pure TwoHalfStar
        | s == "3" -> pure ThreeStar
        | s == "7/2" -> pure ThreeHalfStar
        | s == "4" -> pure FourStar
        | s == "9/2" -> pure FourHalfStar
        | s == "5" -> pure FiveStar
        | otherwise -> fail "Not a Rating"
