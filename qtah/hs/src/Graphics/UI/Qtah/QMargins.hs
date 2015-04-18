module Graphics.UI.Qtah.QMargins (
  QMargins (..),
  null,
  isNull,
  -- * Internal
  encode,
  decode,
  ) where

import Foreign (Ptr, free, newArray, peekArray)
import Foreign.C (CInt)
import Prelude hiding (null)

data QMargins = QMargins
  { left :: CInt
  , top :: CInt
  , right :: CInt
  , bottom :: CInt
  } deriving (Eq)

null :: QMargins
null = QMargins 0 0 0 0

isNull :: QMargins -> Bool
isNull (QMargins 0 0 0 0) = True
isNull _ = False

encode :: QMargins -> IO (Ptr CInt)
encode (QMargins l t r b) = newArray [l, t, r, b]

decode :: Ptr CInt -> IO QMargins
decode p = do
  [l, t, r, b] <- peekArray 4 p
  free p
  return $ QMargins l t r b
