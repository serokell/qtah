module Graphics.UI.Qtah.Core.HPoint (
  HPoint (..),
  null,
  isNull,
  manhattanLength,
  ) where

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
