module Data.ArrayBuffer.Extra where

import Data.ArrayBuffer.Types (Uint8Array)

foreign import newUint8Array :: Array Int -> Uint8Array
