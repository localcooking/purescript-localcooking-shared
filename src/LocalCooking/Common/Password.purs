module LocalCooking.Common.Password where

import Prelude

import Data.Maybe (Maybe (..))
import Data.ArrayBuffer.Types (Uint8Array)
import Data.ArrayBuffer.Base64 (encodeBase64, decodeBase64)
import Data.Argonaut (class EncodeJson, encodeJson, class DecodeJson, decodeJson, fail)
import Data.TextEncoder (encodeUtf8)
import Data.String.Normalize (nfkc)
import Crypto.Scrypt (SCRYPT, scrypt)
import Control.Monad.Aff (Aff)


newtype HashedPassword = HashedPassword Uint8Array

instance encodeJsonHashedPassword :: EncodeJson HashedPassword where
  encodeJson (HashedPassword x) = encodeJson (encodeBase64 x)

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
