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
