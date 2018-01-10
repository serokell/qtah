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

module Graphics.UI.Qtah.Generator.Interface.Core.QAbstractItemModel (
  aModule,
  c_QAbstractItemModel,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportEnum, ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkMethod,
  mkMethod',
  )
import Foreign.Hoppy.Generator.Types (
  bitspaceT,
  boolT,
  enumT,
  intT,
  objT,
  voidT,
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QModelIndex (c_QModelIndex)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Generator.Interface.Core.QVariant (c_QVariant)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (
  bs_ItemFlags,
  e_ItemDataRole,
  e_Orientation,
  e_SortOrder,
  )
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  c_Listener,
  c_ListenerQModelIndexIntInt,
  c_ListenerQModelIndexIntIntQModelIndexInt,
  c_ListenerQModelIndexQModelIndexQVectorInt,
  )
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QAbstractItemModel"] $
  QtExport (ExportClass c_QAbstractItemModel) :
  map QtExportSignal signals ++
  [ QtExport $ ExportEnum e_LayoutChangeHint ]

c_QAbstractItemModel =
  addReqIncludes [includeStd "QAbstractItemModel"] $
  classSetEntityPrefix "" $
  makeClass (ident "QAbstractItemModel") Nothing [c_QObject] $
  collect
  [ just $ mkConstMethod "buddy" [objT c_QModelIndex] $ objT c_QModelIndex
    -- TODO canDropMimeData
  , just $ mkConstMethod "canFetchMore" [objT c_QModelIndex] boolT
  , just $ mkConstMethod' "columnCount" "columnCount" [] intT
  , just $ mkConstMethod' "columnCount" "columnCountAt" [objT c_QModelIndex] intT
  , just $ mkConstMethod' "data" "getData" [objT c_QModelIndex] $ objT c_QVariant
  , just $ mkConstMethod' "data" "getDataWithRole"
    [objT c_QModelIndex, enumT e_ItemDataRole] $ objT c_QVariant
    -- TODO dropMimeData
  , just $ mkMethod "fetchMore" [objT c_QModelIndex] voidT
  , just $ mkConstMethod "flags" [objT c_QModelIndex] $ bitspaceT bs_ItemFlags
  , just $ mkConstMethod' "hasChildren" "hasChildren" [] boolT
  , just $ mkConstMethod' "hasChildren" "hasChildrenAt" [objT c_QModelIndex] boolT
  , just $ mkConstMethod' "hasIndex" "hasIndex" [intT, intT] boolT
  , just $ mkConstMethod' "hasIndex" "hasIndexAt" [intT, intT, objT c_QModelIndex] boolT
  , just $ mkConstMethod' "headerData" "headerData" [intT, enumT e_Orientation] $ objT c_QVariant
  , just $ mkConstMethod' "headerData" "headerDataWithRole"
    [intT, enumT e_Orientation, enumT e_ItemDataRole] $ objT c_QVariant
  , just $ mkConstMethod' "index" "index" [intT, intT] $ objT c_QModelIndex
  , just $ mkConstMethod' "index" "indexAt" [intT, intT, objT c_QModelIndex] $ objT c_QModelIndex
  , just $ mkMethod' "insertColumn" "insertColumn" [intT] boolT
  , just $ mkMethod' "insertColumn" "insertColumnAt" [intT, objT c_QModelIndex] boolT
  , just $ mkMethod' "insertColumns" "insertColumns" [intT, intT] boolT
  , just $ mkMethod' "insertColumns" "insertColumnsAt" [intT, intT, objT c_QModelIndex] boolT
  , just $ mkMethod' "insertRow" "insertRow" [intT] boolT
  , just $ mkMethod' "insertRow" "insertRowAt" [intT, objT c_QModelIndex] boolT
  , just $ mkMethod' "insertRows" "insertRows" [intT, intT] boolT
  , just $ mkMethod' "insertRows" "insertRowsAt" [intT, intT, objT c_QModelIndex] boolT
    -- TODO itemData
    -- TODO match
    -- TODO mimeData
    -- TODO mimeTypes
  , test (qtVersion >= [5, 0]) $ mkMethod "moveColumn"
    [objT c_QModelIndex, intT, objT c_QModelIndex, intT] boolT
  , test (qtVersion >= [5, 0]) $ mkMethod "moveColumns"
    [objT c_QModelIndex, intT, intT, objT c_QModelIndex, intT] boolT
  , test (qtVersion >= [5, 0]) $ mkMethod "moveRow"
    [objT c_QModelIndex, intT, objT c_QModelIndex, intT] boolT
  , test (qtVersion >= [5, 0]) $ mkMethod "moveRows"
    [objT c_QModelIndex, intT, intT, objT c_QModelIndex, intT] boolT
  , just $ mkConstMethod "parent" [objT c_QModelIndex] $ objT c_QModelIndex
  , just $ mkMethod' "removeColumn" "removeColumn" [intT] boolT
  , just $ mkMethod' "removeColumn" "removeColumnAt" [intT, objT c_QModelIndex] boolT
  , just $ mkMethod' "removeColumns" "removeColumns" [intT, intT] boolT
  , just $ mkMethod' "removeColumns" "removeColumnsAt" [intT, intT, objT c_QModelIndex] boolT
  , just $ mkMethod' "removeRow" "removeRow" [intT] boolT
  , just $ mkMethod' "removeRow" "removeRowAt" [intT, objT c_QModelIndex] boolT
  , just $ mkMethod' "removeRows" "removeRows" [intT, intT] boolT
  , just $ mkMethod' "removeRows" "removeRowsAt" [intT, intT, objT c_QModelIndex] boolT
  , just $ mkMethod "revert" [] voidT
    -- TODO roleNames (>=4.6)
  , just $ mkConstMethod' "rowCount" "rowCount" [] intT
  , just $ mkConstMethod' "rowCount" "rowCountAt" [objT c_QModelIndex] intT
  , just $ mkMethod' "setData" "setData" [objT c_QModelIndex, objT c_QVariant] boolT
  , just $ mkMethod' "setData" "setDataWithRole"
    [objT c_QModelIndex, objT c_QVariant, enumT e_ItemDataRole] boolT
  , just $ mkMethod' "setHeaderData" "setHeaderData"
    [intT, enumT e_Orientation, objT c_QVariant] boolT
  , just $ mkMethod' "setHeaderData" "setHeaderDataWithRole"
    [intT, enumT e_Orientation, objT c_QVariant, enumT e_ItemDataRole] boolT
    -- TODO setItemData
  , just $ mkConstMethod "sibling" [intT, intT, objT c_QModelIndex] $ objT c_QModelIndex
  , just $ mkMethod' "sort" "sort" [intT] voidT
  , just $ mkMethod' "sort" "sortWithOrder" [intT, enumT e_SortOrder] voidT
  , just $ mkConstMethod "span" [objT c_QModelIndex] $ objT c_QSize
  , just $ mkMethod "submit" [] boolT
    -- TODO suportedDragActions
    -- TODO supportedDropActions (>=4.2)
  ]

signals =
  collect
  [ just $ makeSignal c_QAbstractItemModel "columnsAboutToBeInserted" c_ListenerQModelIndexIntInt
  , test (qtVersion >= [4, 6]) $ makeSignal c_QAbstractItemModel "columnsAboutToBeMoved"
    c_ListenerQModelIndexIntIntQModelIndexInt
  , just $ makeSignal c_QAbstractItemModel "columnsAboutToBeRemoved" c_ListenerQModelIndexIntInt
  , just $ makeSignal c_QAbstractItemModel "columnsInserted" c_ListenerQModelIndexIntInt
  , test (qtVersion >= [4, 6]) $ makeSignal c_QAbstractItemModel "columnsMoved"
    c_ListenerQModelIndexIntIntQModelIndexInt
  , just $ makeSignal c_QAbstractItemModel "columnsRemoved" c_ListenerQModelIndexIntInt
  , just $ makeSignal c_QAbstractItemModel "dataChanged" c_ListenerQModelIndexQModelIndexQVectorInt
    -- TODO layoutAboutToBeChanged (>=5.0)
    -- TODO layoutChanged (>=5.0)
  , test (qtVersion >= [4, 2]) $ makeSignal c_QAbstractItemModel "modelAboutToBeReset" c_Listener
  , test (qtVersion >= [4, 1]) $ makeSignal c_QAbstractItemModel "modelReset" c_Listener
  , just $ makeSignal c_QAbstractItemModel "rowsAboutToBeInserted" c_ListenerQModelIndexIntInt
  , test (qtVersion >= [4, 6]) $ makeSignal c_QAbstractItemModel "rowsAboutToBeMoved"
    c_ListenerQModelIndexIntIntQModelIndexInt
  , just $ makeSignal c_QAbstractItemModel "rowsAboutToBeRemoved" c_ListenerQModelIndexIntInt
  , just $ makeSignal c_QAbstractItemModel "rowsInserted" c_ListenerQModelIndexIntInt
  , test (qtVersion >= [4, 6]) $ makeSignal c_QAbstractItemModel "rowsMoved"
    c_ListenerQModelIndexIntIntQModelIndexInt
  , just $ makeSignal c_QAbstractItemModel "rowsRemoved" c_ListenerQModelIndexIntInt
  ]

e_LayoutChangeHint =
  makeQtEnum (ident1 "QAbstractItemModel" "LayoutChangeHint") [includeStd "QAbstractItemModel"]
  [ (0, ["no", "layout", "change", "hint"])
  , (1, ["vertical", "sort", "hint"])
  , (2, ["horizontal", "sort", "hint"])
  ]
