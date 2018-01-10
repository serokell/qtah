-- This file is part of Qtah.
--
-- Copyright 2015-2018 The Qtah Authors.
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Graphics.UI.Qtah.Core.HSize (
  HSize (..),
  null,
  isEmpty,
  isNull,
  isValid,
  transpose,
  ) where

import Prelude hiding (null)

data HSize = HSize
  { width :: Int
  , height :: Int
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
