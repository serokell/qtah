-- This file is part of Qtah.
--
-- Copyright 2015-2016 Bryan Gardiner <bog@khumba.net>
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

module Graphics.UI.Qtah.Internal.Interface.Gui (modules) where

import Graphics.UI.Qtah.Internal.Generator.Types
import qualified Graphics.UI.Qtah.Internal.Interface.Gui.QActionEvent as QActionEvent
import qualified Graphics.UI.Qtah.Internal.Interface.Gui.QClipboard as QClipboard
import qualified Graphics.UI.Qtah.Internal.Interface.Gui.QCloseEvent as QCloseEvent
import qualified Graphics.UI.Qtah.Internal.Interface.Gui.QColor as QColor
import qualified Graphics.UI.Qtah.Internal.Interface.Gui.QHoverEvent as QHoverEvent
import qualified Graphics.UI.Qtah.Internal.Interface.Gui.QInputEvent as QInputEvent
import qualified Graphics.UI.Qtah.Internal.Interface.Gui.QKeyEvent as QKeyEvent
import qualified Graphics.UI.Qtah.Internal.Interface.Gui.QMouseEvent as QMouseEvent
import qualified Graphics.UI.Qtah.Internal.Interface.Gui.QWheelEvent as QWheelEvent

{-# ANN module "HLint: ignore Use camelCase" #-}

modules :: [AModule]
modules =
  [ QActionEvent.aModule
  , QClipboard.aModule
  , QCloseEvent.aModule
  , QColor.aModule
  , QHoverEvent.aModule
  , QInputEvent.aModule
  , QKeyEvent.aModule
  , QMouseEvent.aModule
  , QWheelEvent.aModule
  ]
