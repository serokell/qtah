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

module Graphics.UI.Qtah.Generator.Interface.Gui (modules) where

import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QActionEvent as QActionEvent
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QBrush as QBrush
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QClipboard as QClipboard
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QCloseEvent as QCloseEvent
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QColor as QColor
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QDoubleValidator as QDoubleValidator
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QEnterEvent as QEnterEvent
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QExposeEvent as QExposeEvent
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QFocusEvent as QFocusEvent
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QFont as QFont
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QHideEvent as QHideEvent
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QHoverEvent as QHoverEvent
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QIcon as QIcon
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QImage as QImage
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QInputEvent as QInputEvent
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QIntValidator as QIntValidator
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QKeyEvent as QKeyEvent
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QMouseEvent as QMouseEvent
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QOpenGLWindow as QOpenGLWindow
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QPaintDevice as QPaintDevice
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QPaintDeviceWindow as QPaintDeviceWindow
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QPaintEvent as QPaintEvent
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QPainter as QPainter
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QPainterPath as QPainterPath
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QPen as QPen
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QPixmap as QPixmap
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QPolygon as QPolygon
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QPolygonF as QPolygonF
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QRasterWindow as QRasterWindow
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QRegion as QRegion
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QShowEvent as QShowEvent
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QStandardItemModel as QStandardItemModel
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QSurface as QSurface
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QTransform as QTransform
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QValidator as QValidator
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QWheelEvent as QWheelEvent
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QWindow as QWindow
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QtahOpenGLWindow as QtahOpenGLWindow
import qualified Graphics.UI.Qtah.Generator.Interface.Gui.QtahRasterWindow as QtahRasterWindow
import Graphics.UI.Qtah.Generator.Module (AModule)

{-# ANN module "HLint: ignore Use camelCase" #-}

modules :: [AModule]
modules =
  [ QActionEvent.aModule
  , QBrush.aModule
  , QClipboard.aModule
  , QCloseEvent.aModule
  , QColor.aModule
  , QDoubleValidator.aModule
  , QEnterEvent.aModule
  , QExposeEvent.aModule
  , QFocusEvent.aModule
  , QFont.aModule
  , QHideEvent.aModule
  , QHoverEvent.aModule
  , QIcon.aModule
  , QImage.aModule
  , QInputEvent.aModule
  , QIntValidator.aModule
  , QKeyEvent.aModule
  , QMouseEvent.aModule
  , QOpenGLWindow.aModule
  , QPaintDevice.aModule
  , QPaintDeviceWindow.aModule
  , QPaintEvent.aModule
  , QPainter.aModule
  , QPainterPath.aModule
  , QPen.aModule
  , QPixmap.aModule
  , QPolygon.aModule
  , QPolygonF.aModule
  , QRasterWindow.aModule
  , QRegion.aModule
  , QShowEvent.aModule
  , QStandardItemModel.aModule
  , QStandardItemModel.itemModule
  , QStandardItemModel.itemListModule
  , QSurface.aModule
  , QTransform.aModule
  , QValidator.aModule
  , QWheelEvent.aModule
  , QWindow.aModule
  , QtahOpenGLWindow.aModule
  , QtahRasterWindow.aModule
  ]
