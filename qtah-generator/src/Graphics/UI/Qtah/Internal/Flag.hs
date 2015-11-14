-- This file is part of Qtah.
--
-- Copyright 2015 Bryan Gardiner <bog@khumba.net>
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

-- | Utilities for conditional compilation of parts of Qt interfaces.
module Graphics.UI.Qtah.Internal.Flag (
  collect,
  just,
  test,
  ) where

import Data.Maybe (catMaybes)

collect :: [Maybe a] -> [a]
collect = catMaybes

just :: a -> Maybe a
just = Just

test :: Bool -> a -> Maybe a
test True = Just
test False = const Nothing
