module Test.Main where

import LocalCooking.Common.Password (HashedPassword)
import LocalCooking.Common.AccessToken (AccessToken)

import Prelude
import Data.Either (Either (..))
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.QuickCheck (quickCheck)



main :: Eff _ Unit
main = do
  log "JSON Iso:"
  log "    HashedPassword:"
  quickCheck (\(x :: HashedPassword) -> jsonIso x)
  log ""
  log "    AccessToken:"
  quickCheck (\(x :: AccessToken) -> jsonIso x)



jsonIso :: forall a. EncodeJson a => DecodeJson a => Eq a => a -> Boolean
jsonIso x = case decodeJson (encodeJson x) of
  Left _ -> false
  Right y -> x == y
