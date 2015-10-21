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
