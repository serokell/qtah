module Graphics.UI.Qtah.Core.HSize (
  HSize (..),
  null,
  isEmpty,
  isNull,
  isValid,
  transpose,
  -- * Internal
  encodeInternal,
  decodeInternal,
  ) where

import Foreign (Ptr, free, newArray, peekArray)
import Foreign.C (CInt)
import Prelude hiding (null)

data HSize = HSize
  { width :: CInt
  , height :: CInt
  } deriving (Eq, Show)

null :: HSize
null = HSize 0 0

isEmpty :: HSize -> Bool
isEmpty (HSize w h) = w <= 0 || h <= 0

isNull :: HSize -> Bool
isNull (HSize 0 0) = True
isNull _ = False

isValid :: HSize -> Bool
isValid (HSize w h) = w > 0 && h > 0

transpose :: HSize -> HSize
transpose (HSize w h) = HSize h w

encodeInternal :: HSize -> IO (Ptr CInt)
encodeInternal (HSize x y) = newArray [x, y]

decodeInternal :: Ptr CInt -> IO HSize
decodeInternal p = do
  [x, y] <- peekArray 4 p
  free p
  return $ HSize x y
