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
