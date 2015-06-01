module Graphics.UI.Qtah.Core.HRect (
  HRect (..),
  null,
  isNull,
  ) where

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
