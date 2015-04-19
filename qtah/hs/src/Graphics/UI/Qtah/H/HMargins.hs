module Graphics.UI.Qtah.H.HMargins (
  HMargins (..),
  null,
  isNull,
  -- * Internal
  encode,
  decode,
  ) where

import Foreign (Ptr, free, newArray, peekArray)
import Foreign.C (CInt)
import Prelude hiding (null)

data HMargins = HMargins
  { left :: CInt
  , top :: CInt
  , right :: CInt
  , bottom :: CInt
  } deriving (Eq)

null :: HMargins
null = HMargins 0 0 0 0

isNull :: HMargins -> Bool
isNull (HMargins 0 0 0 0) = True
isNull _ = False

encode :: HMargins -> IO (Ptr CInt)
encode (HMargins l t r b) = newArray [l, t, r, b]

decode :: Ptr CInt -> IO HMargins
decode p = do
  [l, t, r, b] <- peekArray 4 p
  free p
  return $ HMargins l t r b
