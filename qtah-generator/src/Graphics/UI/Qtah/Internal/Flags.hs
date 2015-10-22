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

{-# LANGUAGE CPP #-}

-- | Haskell definitions for preprocessor flags that Qt uses for conditional
-- compilation.
--
-- A list of flags enabled on your system can be obtained with:
--
-- > gcc -dM -E $(pkg-config --cflags QtCore) /usr/include/qt4/Qt/qconfig.h | grep '#define QT'
--
-- Using @qglobal.h@ and @#define Q@ provides additional defintions,
-- e.g. version and windowing system information.
module Graphics.UI.Qtah.Internal.Flags (
  qtVersion,
  keypadNavigation,
  qdoc,
  wsWince,
  ) where

-- | The version of Qt that Qtah is being built against.  This controls which
-- functions and types are made available in the API.
qtVersion :: [Int]
qtVersion = [QT_MAJOR, QT_MINOR]

keypadNavigation :: Bool
keypadNavigation = False

qdoc :: Bool
qdoc = False

wsWince :: Bool
wsWince = False
