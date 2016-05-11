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

module Graphics.UI.Qtah.Internal.Interface.Core (modules) where

import Graphics.UI.Qtah.Internal.Generator.Types
import qualified Graphics.UI.Qtah.Internal.Interface.Core.QChar as QChar
import qualified Graphics.UI.Qtah.Internal.Interface.Core.QChildEvent as QChildEvent
import qualified Graphics.UI.Qtah.Internal.Interface.Core.QCoreApplication as QCoreApplication
import qualified Graphics.UI.Qtah.Internal.Interface.Core.QDir as QDir
import qualified Graphics.UI.Qtah.Internal.Interface.Core.QEvent as QEvent
import qualified Graphics.UI.Qtah.Internal.Interface.Core.QList as QList
import qualified Graphics.UI.Qtah.Internal.Interface.Core.QMargins as QMargins
import qualified Graphics.UI.Qtah.Internal.Interface.Core.QMarginsF as QMarginsF
import qualified Graphics.UI.Qtah.Internal.Interface.Core.QObject as QObject
import qualified Graphics.UI.Qtah.Internal.Interface.Core.QPoint as QPoint
import qualified Graphics.UI.Qtah.Internal.Interface.Core.QPointF as QPointF
import qualified Graphics.UI.Qtah.Internal.Interface.Core.QRect as QRect
import qualified Graphics.UI.Qtah.Internal.Interface.Core.QRectF as QRectF
import qualified Graphics.UI.Qtah.Internal.Interface.Core.QSize as QSize
import qualified Graphics.UI.Qtah.Internal.Interface.Core.QSizeF as QSizeF
import qualified Graphics.UI.Qtah.Internal.Interface.Core.QString as QString
import qualified Graphics.UI.Qtah.Internal.Interface.Core.QStringList as QStringList
import qualified Graphics.UI.Qtah.Internal.Interface.Core.QTimerEvent as QTimerEvent
import qualified Graphics.UI.Qtah.Internal.Interface.Core.QVector as QVector
import qualified Graphics.UI.Qtah.Internal.Interface.Core.Types as Types

{-# ANN module "HLint: ignore Use camelCase" #-}

modules :: [AModule]
modules =
  concat
  [ [ QChar.aModule
    , QChildEvent.aModule
    , QCoreApplication.aModule
    , QDir.aModule
    , QEvent.aModule
    , QMargins.aModule
    , QMarginsF.aModule
    , QObject.aModule
    , QPoint.aModule
    , QPointF.aModule
    , QRect.aModule
    , QRectF.aModule
    , QSize.aModule
    , QSizeF.aModule
    , QString.aModule
    , QStringList.aModule
    , QTimerEvent.aModule
    , Types.aModule
    ]
  , QList.allModules
  , QVector.allModules
  ]
