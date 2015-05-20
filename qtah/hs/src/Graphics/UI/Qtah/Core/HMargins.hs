module Graphics.UI.Qtah.Core.HMargins (
  HMargins (..),
  null,
  isNull,
  -- * Internal
  encodeInternal,
  decodeInternal,
  ) where

import Foreign (Ptr, free, newArray, peekArray)
import Foreign.C (CInt)
import Prelude hiding (null)

data HMargins = HMargins
  { left :: CInt
  , top :: CInt
  , right :: CInt
  , bottom :: CInt
  } deriving (Eq, Show)

null :: HMargins
null = HMargins 0 0 0 0

isNull :: HMargins -> Bool
isNull (HMargins 0 0 0 0) = True
isNull _ = False

encodeInternal :: HMargins -> IO (Ptr CInt)
encodeInternal (HMargins l t r b) = newArray [l, t, r, b]

decodeInternal :: Ptr CInt -> IO HMargins
decodeInternal p = do
  [l, t, r, b] <- peekArray 4 p
  free p
  return $ HMargins l t r b
