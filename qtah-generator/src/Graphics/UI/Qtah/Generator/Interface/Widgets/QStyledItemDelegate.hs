-- This file is part of Qtah.
--
-- Copyright 2018 The Qtah Authors.
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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QStyledItemDelegate (
  aModule,
  c_QStyledItemDelegate,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportBitspace, ExportEnum, ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkBoolHasProp,
  mkConstMethod,
  mkMethod,
  mkMethod',
  mkProp,
  )
import Foreign.Hoppy.Generator.Types (
  bitspaceT,
  boolT,
  enumT,
  intT,
  objT,
  ptrT,
  voidT,
  )
import Graphics.UI.Qtah.Generator.Interface.Core.QAbstractItemModel (c_QAbstractItemModel)
import Graphics.UI.Qtah.Generator.Interface.Core.QItemSelectionModel (c_QItemSelectionModel)
import Graphics.UI.Qtah.Generator.Interface.Core.QModelIndex (c_QModelIndex)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (
  e_DropAction,
  e_TextElideMode,
  )
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  c_Listener,
  c_ListenerQModelIndex,
  c_ListenerQSize,
  )
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractItemDelegate (
  c_QAbstractItemDelegate,
  )
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractScrollArea (c_QAbstractScrollArea)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (
  AModule (AQtModule), makeQtModule, makeQtModuleWithMinVersion,
  )
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

minVersion = [4, 4]

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Widgets", "QStyledItemDelegate"] minVersion $
  QtExport (ExportClass c_QStyledItemDelegate) :
  map QtExportSignal signals ++
  [
  ]

c_QStyledItemDelegate =
  addReqIncludes [includeStd "QStyledItemDelegate"] $
  classSetEntityPrefix "" $
  makeClass (ident "QStyledItemDelegate") Nothing [c_QAbstractItemDelegate] $
  [
  ]

signals =
  [
  ]
