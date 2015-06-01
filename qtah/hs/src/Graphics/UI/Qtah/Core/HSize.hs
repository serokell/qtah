module Graphics.UI.Qtah.Core.HSize (
  HSize (..),
  null,
  isEmpty,
  isNull,
  isValid,
  transpose,
  ) where

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
