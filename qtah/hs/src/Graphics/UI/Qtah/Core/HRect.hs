module Graphics.UI.Qtah.Core.HRect (
  HRect (..),
  null,
  isNull,
  -- * Internal
  encodeInternal,
  decodeInternal,
  ) where

import Foreign (Ptr, free, newArray, peekArray)
import Foreign.C (CInt)
import Prelude hiding (null)

data HRect = HRect
  { x :: CInt
  , y :: CInt
  , width :: CInt
  , height :: CInt
  } deriving (Eq, Show)

null :: HRect
null = HRect 0 0 0 0

isNull :: HRect -> Bool
isNull (HRect _ _ w h) = w == 0 && h == 0

encodeInternal :: HRect -> IO (Ptr CInt)
encodeInternal (HRect x y w h) = newArray [x, y, w, h]

decodeInternal :: Ptr CInt -> IO HRect
decodeInternal p = do
  [x, y, w, h] <- peekArray 4 p
  free p
  return $ HRect x y w h
