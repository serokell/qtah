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

module Graphics.UI.Qtah.Generator.Interface.Core (modules) where

import qualified Graphics.UI.Qtah.Generator.Interface.Core.QAbstractItemModel as QAbstractItemModel
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QAbstractListModel as QAbstractListModel
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QAbstractTableModel as QAbstractTableModel
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QByteArray as QByteArray
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QChar as QChar
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QChildEvent as QChildEvent
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QCoreApplication as QCoreApplication
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QDate as QDate
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QDir as QDir
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QEvent as QEvent
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QItemSelection as QItemSelection
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QItemSelectionModel as QItemSelectionModel
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QItemSelectionRange as QItemSelectionRange
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QList as QList
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QMargins as QMargins
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QMarginsF as QMarginsF
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QModelIndex as QModelIndex
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QObject as QObject
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QPersistentModelIndex as QPersistentModelIndex
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QPoint as QPoint
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QPointF as QPointF
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QRect as QRect
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QRectF as QRectF
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QSettings as QSettings
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QSize as QSize
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QSizeF as QSizeF
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QString as QString
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QStringList as QStringList
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QStringListModel as QStringListModel
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QTextCodec as QTextCodec
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QTimer as QTimer
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QTimerEvent as QTimerEvent
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QVariant as QVariant
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QVector as QVector
import qualified Graphics.UI.Qtah.Generator.Interface.Core.Types as Types
import Graphics.UI.Qtah.Generator.Module (AModule)

{-# ANN module "HLint: ignore Use camelCase" #-}

modules :: [AModule]
modules =
  concat
  [ [ QAbstractItemModel.aModule
    , QAbstractListModel.aModule
    , QAbstractTableModel.aModule
    , QByteArray.aModule
    , QChar.aModule
    , QChildEvent.aModule
    , QCoreApplication.aModule
    , QDate.aModule
    , QDir.aModule
    , QEvent.aModule
    , QItemSelection.aModule
    , QItemSelectionModel.aModule
    , QItemSelectionRange.aModule
    , QMargins.aModule
    , QMarginsF.aModule
    , QModelIndex.aModule
    , QObject.aModule
    , QPersistentModelIndex.aModule
    , QPoint.aModule
    , QPointF.aModule
    , QRect.aModule
    , QRectF.aModule
    , QSettings.aModule
    , QSize.aModule
    , QSizeF.aModule
    , QString.aModule
    , QStringList.aModule
    , QStringListModel.aModule
    , QTextCodec.aModule
    , QTimer.aModule
    , QTimerEvent.aModule
    , QVariant.aModule
    , Types.aModule
    ]
  , QList.allModules
  , QVector.allModules
  ]
