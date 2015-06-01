module Graphics.UI.Qtah.Core.HMargins (
  HMargins (..),
  null,
  isNull,
  ) where

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
