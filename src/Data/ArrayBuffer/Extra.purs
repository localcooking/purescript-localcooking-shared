module Data.ArrayBuffer.Extra where

import Data.ArrayBuffer.Types (Uint8Array)

foreign import newUint8Array :: Array Int -> Uint8Array

foreign import uint8ArrayToArray :: Uint8Array -> Array Int
