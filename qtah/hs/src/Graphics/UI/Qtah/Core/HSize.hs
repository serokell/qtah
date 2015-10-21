-- This file is part of Qtah.
--
-- Copyright 2015 Bryan Gardiner <bog@khumba.net>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License version 3
-- as published by the Free Software Foundation.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
