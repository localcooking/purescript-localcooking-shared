module LocalCooking.Common.User.Password where

import Prelude

import Data.Maybe (Maybe (..))
import Data.ArrayBuffer.Types (Uint8Array)
import Data.ArrayBuffer.Base64 (encodeBase64, decodeBase64)
import Data.ArrayBuffer.Extra (newUint8Array, uint8ArrayToArray)
import Data.Argonaut (class EncodeJson, encodeJson, class DecodeJson, decodeJson, fail)
import Data.TextEncoder (encodeUtf8)
import Data.String.Normalize (nfkc)
import Data.Generic (class Generic, toSpine, fromSpine, GenericSignature (SigProd))
import Type.Proxy (Proxy (..))
import Crypto.Scrypt (SCRYPT, scrypt)
import Control.Monad.Aff (Aff)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (chooseInt, vectorOf)


newtype HashedPassword = HashedPassword Uint8Array

instance genericHashedPassword :: Generic HashedPassword where
  toSpine (HashedPassword xs) = toSpine (uint8ArrayToArray xs)
  toSignature Proxy = SigProd "HashedPassword" []
  fromSpine xs = (HashedPassword <<< newUint8Array) <$> fromSpine xs

instance eqHashedPassword :: Eq HashedPassword where
  eq (HashedPassword x) (HashedPassword y) = encodeBase64 x == encodeBase64 y

instance arbitraryHashedPassword :: Arbitrary HashedPassword where
  arbitrary = do
    xs <- vectorOf 32 byte
    pure (HashedPassword (newUint8Array xs))
    where
      byte = chooseInt 0 255

instance showHashedPassword :: Show HashedPassword where
  show (HashedPassword x) = encodeBase64 x

instance encodeJsonHashedPassword :: EncodeJson HashedPassword where
  encodeJson = encodeJson <<< show

instance decodeJsonHashedPassword :: DecodeJson HashedPassword where
  decodeJson json = do
    s <- decodeJson json
    case decodeBase64 s of
      Nothing -> fail "Not a HashedPassword"
      Just x -> pure (HashedPassword x)


hashPassword :: forall eff
              . { salt :: HashedPassword
                , password :: String
                } -> Aff (scrypt :: SCRYPT | eff) HashedPassword
hashPassword {salt: HashedPassword salt, password} =
  HashedPassword <$> scrypt
    { password: encodeUtf8 (nfkc password)
    , salt
    , n: 32768 -- https://blog.filippo.io/the-scrypt-parameters/
    , r: 8
    , p: 1
    , dkLen: 32
    , onProgress: \_ -> pure unit
    }
