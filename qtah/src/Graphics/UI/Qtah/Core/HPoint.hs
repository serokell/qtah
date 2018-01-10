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

module Graphics.UI.Qtah.Core.HPoint (
  HPoint (..),
  null,
  isNull,
  manhattanLength,
  ) where

import Prelude hiding (null)

data HPoint = HPoint
  { x :: Int
  , y :: Int
  } deriving (Eq, Ord, Show)

null :: HPoint
null = HPoint 0 0

isNull :: HPoint -> Bool
isNull (HPoint 0 0) = True
isNull _ = False

manhattanLength :: HPoint -> Int
manhattanLength (HPoint x y) = abs x + abs y
