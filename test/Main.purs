module Test.Main where

import Data.Address (USAAddress)
import Data.Image.Source (ImageSource)
import Data.List.Pagination (PaginationArgs)
import Data.List.Sorting (SortingArgs)
import Data.Price (Price)
import Data.String.Markdown (MarkdownText)
import Data.String.Permalink (Permalink)
import Data.Date.JSON (JSONDate)
import Data.DateTime.JSON (JSONDateTime)

import LocalCooking.Common.AccessToken (AccessToken)
import LocalCooking.Common.Diet (Diet)
import LocalCooking.Common.Ingredient (Ingredient)
import LocalCooking.Common.Order (OrderProgress)
import LocalCooking.Common.Rating (Rating)
import LocalCooking.Common.Tag (Tag)
import LocalCooking.Common.User.Name (Name)
import LocalCooking.Common.User.Password (HashedPassword)
import LocalCooking.Common.User.Role (UserRole)

import LocalCooking.Semantics.Common (SocialLoginForm, User, Register, Login, SocialLogin)

import Prelude
import Data.Either (Either (..))
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Type.Proxy (Proxy (..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.QuickCheck (class Arbitrary, quickCheck, Result (..))



main :: Eff _ Unit
main = do
  log "JSON Iso:"
  jsonIsoAssert "Data.Address" (Proxy :: Proxy USAAddress)
  jsonIsoAssert "Data.Image.Source" (Proxy :: Proxy ImageSource)
  jsonIsoAssert "Data.List.Pagination" (Proxy :: Proxy PaginationArgs)
  jsonIsoAssert "Data.List.Sorting" (Proxy :: Proxy (SortingArgs Unit))
  jsonIsoAssert "Data.Price" (Proxy :: Proxy Price)
  jsonIsoAssert "Data.String.Markdown" (Proxy :: Proxy MarkdownText)
  jsonIsoAssert "Data.String.Permalink" (Proxy :: Proxy Permalink)
  jsonIsoAssert "Data.Date.JSON" (Proxy :: Proxy JSONDate)
  jsonIsoAssert "Data.DateTime.JSON" (Proxy :: Proxy JSONDateTime)
  log "--------"
  jsonIsoAssert "LocalCooking.Common.AccessToken" (Proxy :: Proxy AccessToken)
  jsonIsoAssert "LocalCooking.Common.Diet" (Proxy :: Proxy Diet)
  jsonIsoAssert "LocalCooking.Common.Ingredient" (Proxy :: Proxy Ingredient)
  jsonIsoAssert "LocalCooking.Common.Order" (Proxy :: Proxy OrderProgress)
  jsonIsoAssert "LocalCooking.Common.Rating" (Proxy :: Proxy Rating)
  jsonIsoAssert "LocalCooking.Common.Tag" (Proxy :: Proxy Tag)
  jsonIsoAssert "LocalCooking.Common.User.Name" (Proxy :: Proxy Name)
  jsonIsoAssert "LocalCooking.Common.User.Password" (Proxy :: Proxy HashedPassword)
  jsonIsoAssert "LocalCooking.Common.User.Role" (Proxy :: Proxy UserRole)
  log "--------"
  jsonIsoAssert "LocalCooking.Semantics.Common.SocialLoginForm" (Proxy :: Proxy SocialLoginForm)
  jsonIsoAssert "LocalCooking.Semantics.Common.User" (Proxy :: Proxy User)
  jsonIsoAssert "LocalCooking.Semantics.Common.Register" (Proxy :: Proxy Register)
  jsonIsoAssert "LocalCooking.Semantics.Common.Login" (Proxy :: Proxy Login)
  jsonIsoAssert "LocalCooking.Semantics.Common.SocialLogin" (Proxy :: Proxy SocialLogin)


jsonIsoAssert :: forall a
               . EncodeJson a
              => DecodeJson a
              => Eq a
              => Show a
              => Arbitrary a
              => String -> Proxy a -> Eff _ Unit
jsonIsoAssert name Proxy = do
  log ("    " <> name)
  quickCheck (\(x :: a) -> jsonIso x)
  log ""


jsonIso :: forall a. EncodeJson a => DecodeJson a => Eq a => Show a => a -> Result
jsonIso x = case decodeJson (encodeJson x) of
  Left x -> Failed $ "decoding failure: " <> x <> ", " <> show x
  Right y
    | x == y -> Success
    | otherwise -> Failed $ "Not identical: " <> show x <> ", " <> show y
