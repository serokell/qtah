module Graphics.UI.Qtah.H.HPoint (
  HPoint (..),
  null,
  isNull,
  manhattanLength,
  -- * Internal
  encodeInternal,
  decodeInternal,
  ) where

import Foreign (Ptr, free, newArray, peekArray)
import Foreign.C (CInt)
import Prelude hiding (null)

data HPoint = HPoint
  { x :: CInt
  , y :: CInt
  } deriving (Eq, Show)

null :: HPoint
null = HPoint 0 0

isNull :: HPoint -> Bool
isNull (HPoint 0 0) = True
isNull _ = False

manhattanLength :: HPoint -> CInt
manhattanLength (HPoint x y) = abs x + abs y

encodeInternal :: HPoint -> IO (Ptr CInt)
encodeInternal (HPoint x y) = newArray [x, y]

decodeInternal :: Ptr CInt -> IO HPoint
decodeInternal p = do
  [x, y] <- peekArray 4 p
  free p
  return $ HPoint x y
