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

module Graphics.UI.Qtah.Generator.Interface.Core.QItemSelectionModel (
  aModule,
  c_QItemSelectionModel,
  bs_SelectionFlags,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportBitspace, ExportEnum, ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkMethod',
  )
import Foreign.Hoppy.Generator.Types (
  bitspaceT,
  boolT,
  constT,
  intT,
  objT,
  ptrT,
  voidT,
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QAbstractItemModel (c_QAbstractItemModel)
import Graphics.UI.Qtah.Generator.Interface.Core.QItemSelection (c_QItemSelection)
import Graphics.UI.Qtah.Generator.Interface.Core.QList (c_QListQModelIndex)
import Graphics.UI.Qtah.Generator.Interface.Core.QModelIndex (c_QModelIndex)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  c_ListenerPtrQAbstractItemModel,
  c_ListenerRefConstQItemSelectionRefConstQItemSelection,
  c_ListenerQModelIndexQModelIndex,
  )
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QItemSelectionModel"] $
  [ QtExport $ ExportClass c_QItemSelectionModel ] ++
  map QtExportSignal signals ++
  [ QtExport $ ExportEnum e_SelectionFlag
  , QtExport $ ExportBitspace bs_SelectionFlags
  ]

c_QItemSelectionModel =
  addReqIncludes [includeStd "QItemSelectionModel"] $
  classSetEntityPrefix "" $
  makeClass (ident "QItemSelectionModel") Nothing [c_QObject] $
  collect
  [ -- The nullary constructor requires at least Qt >=5.0.  Release notes[1]
    -- point at QItemSelectionModel gaining some methods in 5.5:
    --
    --     QAbstractItemModel* model()
    --     void modelChanged(QAbstractItemModel* model)
    --     void setModel(QAbstractItemModel* model)
    --
    -- So I suspect that this constructor appeared at that point.
    --
    -- [1] https://doc.qt.io/qt-5/newclasses55.html
    test (qtVersion >= [5, 5]) $ mkCtor "new" []
  , just $ mkCtor "newWithModel" [ptrT $ objT c_QAbstractItemModel]
  , just $ mkCtor "newWithModelAndParent" [ptrT $ objT c_QAbstractItemModel, ptrT $ objT c_QObject]
  , just $ mkMethod "clear" [] voidT
  , test (qtVersion >= [5, 0]) $ mkMethod "clearCurrentIndex" [] voidT
  , just $ mkMethod "clearSelection" [] voidT
  , just $ mkConstMethod "columnIntersectsSelection" [intT, objT c_QModelIndex] boolT
  , just $ mkConstMethod "currentIndex" [] $ objT c_QModelIndex
  , just $ mkConstMethod "hasSelection" [] boolT
  , just $ mkConstMethod "isColumnSelected" [intT, objT c_QModelIndex] boolT
  , just $ mkConstMethod "isRowSelected" [intT, objT c_QModelIndex] boolT
  , just $ mkConstMethod "isSelected" [objT c_QModelIndex] boolT
  , test (qtVersion >= [5, 5]) $ mkMethod' "model" "model" [] $ ptrT $ objT c_QAbstractItemModel
  , just $ mkConstMethod' "model" "modelConst" [] $ ptrT $ constT $ objT c_QAbstractItemModel
  , just $ mkMethod "reset" [] voidT
  , just $ mkConstMethod "rowIntersectsSelection" [intT, objT c_QModelIndex] boolT
  , just $ mkMethod' "select" "selectIndex" [objT c_QModelIndex, bitspaceT bs_SelectionFlags] voidT
  , just $ mkMethod' "select" "selectSelection" [objT c_QItemSelection, bitspaceT bs_SelectionFlags]
    voidT
  , just $ mkConstMethod "selectedColumns" [intT] $ objT c_QListQModelIndex
  , test (qtVersion >= [5, 5]) $ mkConstMethod "selectedIndexes" [] $ objT c_QListQModelIndex
  , just $ mkConstMethod "selectedRows" [intT] $ objT c_QListQModelIndex
  , just $ mkConstMethod "selection" [] $ objT c_QItemSelection
  , just $ mkMethod "setCurrentIndex" [objT c_QModelIndex, bitspaceT bs_SelectionFlags] voidT
  , test (qtVersion >= [5, 5]) $ mkMethod "setModel" [ptrT $ objT c_QAbstractItemModel] voidT
  ]

signals =
  collect
  [ just $ makeSignal c_QItemSelectionModel "currentChanged" c_ListenerQModelIndexQModelIndex
  , just $ makeSignal c_QItemSelectionModel "currentColumnChanged" c_ListenerQModelIndexQModelIndex
  , just $ makeSignal c_QItemSelectionModel "currentRowChanged" c_ListenerQModelIndexQModelIndex
  , test (qtVersion >= [5, 5]) $ makeSignal c_QItemSelectionModel "modelChanged"
    c_ListenerPtrQAbstractItemModel
  , just $ makeSignal c_QItemSelectionModel "selectionChanged"
    c_ListenerRefConstQItemSelectionRefConstQItemSelection
  ]

(e_SelectionFlag, bs_SelectionFlags) =
  makeQtEnumBitspace (ident1 "QItemSelectionModel" "SelectionFlag") "SelectionFlags"
  [includeStd "QItemSelectionModel"]
  [ (0x00, ["no", "update"])
    -- Renamed from just "clear" because it clashes with the "clear" method.
  , (0x01, ["clear", "flag"])
  , (0x02, ["select"])
  , (0x04, ["deselect"])
  , (0x08, ["toggle"])
  , (0x10, ["current"])
  , (0x20, ["rows"])
  , (0x40, ["columns"])
  , (0x12, ["select", "current"])
  , (0x18, ["toggle", "current"])
  , (0x03, ["clear", "and", "select"])
  ]
