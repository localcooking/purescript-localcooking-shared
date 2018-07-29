module LocalCooking.Semantics.User where

import Prelude
import Data.NonEmpty (NonEmpty (..))
import Data.Generic (class Generic, gEq, gShow)
import Data.Argonaut
  ( class EncodeJson, class DecodeJson, encodeJson, decodeJson
  , fail, (:=), (~>), jsonEmptyObject, (.?))
import Control.Alternative ((<|>))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)



data UserExists a
  = UserDoesntExist
  | UserExists a

derive instance genericUserExists :: Generic a => Generic (UserExists a)

instance arbitraryUserExists :: Arbitrary a => Arbitrary (UserExists a) where
  arbitrary = oneOf $ NonEmpty
    ( pure UserDoesntExist
    )
    [ UserExists <$> arbitrary
    ]

instance eqUserExists :: Generic a => Eq (UserExists a) where
  eq = gEq

instance showUserExists :: Generic a => Show (UserExists a) where
  show = gShow

instance encodeJsonUserExists :: EncodeJson a => EncodeJson (UserExists a) where
  encodeJson x = case x of
    UserDoesntExist -> encodeJson "userDoesntExist"
    UserExists y
      -> "userExists"
      := y
      ~> jsonEmptyObject

instance decodeJsonUserExists :: DecodeJson a => DecodeJson (UserExists a) where
  decodeJson json = do
    let empty = do
          s <- decodeJson json
          if s == "userDoesntExist"
             then pure UserDoesntExist
             else fail "Not a UserExists"
        has = do
          o <- decodeJson json
          UserExists <$> o .? "userExists"
    empty <|> has



data HasRole a
  = DoesntHaveRole
  | HasRole a

derive instance genericHasRole :: Generic a => Generic (HasRole a)

instance arbitraryHasRole :: Arbitrary a => Arbitrary (HasRole a) where
  arbitrary = oneOf $ NonEmpty
    ( pure DoesntHaveRole
    )
    [ HasRole <$> arbitrary
    ]

instance eqHasRole :: Generic a => Eq (HasRole a) where
  eq = gEq

instance showHasRole :: Generic a => Show (HasRole a) where
  show = gShow

instance encodeJsonHasRole :: EncodeJson a => EncodeJson (HasRole a) where
  encodeJson x = case x of
    DoesntHaveRole -> encodeJson "doesntHaveRole"
    HasRole y
      -> "hasRole"
      := y
      ~> jsonEmptyObject

instance decodeJsonHasRole :: DecodeJson a => DecodeJson (HasRole a) where
  decodeJson json = do
    let empty = do
          s <- decodeJson json
          if s == "doesntHaveRole"
             then pure DoesntHaveRole
             else fail "Not a HasRole"
        has = do
          o <- decodeJson json
          HasRole <$> o .? "hasRole"
    empty <|> has



data UserUnique a
  = UserNotUnique
  | UserUnique a

derive instance genericUserUnique :: Generic a => Generic (UserUnique a)

instance arbitraryUserUnique :: Arbitrary a => Arbitrary (UserUnique a) where
  arbitrary = oneOf $ NonEmpty
    ( pure UserNotUnique
    )
    [ UserUnique <$> arbitrary
    ]

instance eqUserUnique :: Generic a => Eq (UserUnique a) where
  eq = gEq

instance showUserUnique :: Generic a => Show (UserUnique a) where
  show = gShow

instance encodeJsonUserUnique :: EncodeJson a => EncodeJson (UserUnique a) where
  encodeJson x = case x of
    UserNotUnique -> encodeJson "userNotUnique"
    UserUnique y
      -> "userUnique"
      := y
      ~> jsonEmptyObject

instance decodeJsonUserUnique :: DecodeJson a => DecodeJson (UserUnique a) where
  decodeJson json = do
    let empty = do
          s <- decodeJson json
          if s == "userNotUnique"
             then pure UserNotUnique
             else fail "Not a UserUnique"
        has = do
          o <- decodeJson json
          UserUnique <$> o .? "userUnique"
    empty <|> has
