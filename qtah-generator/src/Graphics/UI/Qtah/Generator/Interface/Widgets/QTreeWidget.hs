-- This file is part of Qtah.
--
-- Copyright 2015-2017 The Qtah Authors.
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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QTreeWidget (
  aModule,
  itemModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Class,
  Export (ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  )
import Foreign.Hoppy.Generator.Types (intT, objT, ptrT, voidT, boolT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QStringList (c_QStringList)
import Graphics.UI.Qtah.Generator.Interface.Gui.QIcon (c_QIcon)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QTreeView (c_QTreeView)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule :: AModule
aModule =
  AQtModule $
  makeQtModule ["Widgets", "QTreeWidget"] $
  QtExport (ExportClass c_QTreeWidget) :
  map QtExportSignal signals

itemModule :: AModule
itemModule =
  AQtModule $
  makeQtModule ["Widgets", "QTreeWidgetItem"] $
  QtExport (ExportClass c_QTreeWidgetItem) :
  map QtExportSignal itemSignals

c_QTreeWidget :: Class
c_QTreeWidget =
  addReqIncludes [includeStd "QTreeWidget"] $
  classSetEntityPrefix "" $
  makeClass (ident "QTreeWidget") Nothing [c_QTreeView] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , test (qtVersion >= [4, 1]) $
      mkMethod "addTopLevelItem" [ptrT $ objT c_QTreeWidgetItem] voidT
  , just $ mkConstMethod "currentItem" [] (ptrT $ objT c_QTreeWidgetItem)
  , just $ mkConstMethod "headerItem" [] (ptrT $ objT c_QTreeWidgetItem)
  , just $ mkMethod "setCurrentItem" [ptrT $ objT c_QTreeWidgetItem] voidT
  , just $ mkMethod "setHeaderItem" [ptrT $ objT c_QTreeWidgetItem] voidT
  , test (qtVersion >= [4, 2]) $
    mkMethod "setHeaderLabel" [objT c_QString] voidT
  , just $ mkMethod "setHeaderLabels" [objT c_QStringList] voidT
  , just $ mkConstMethod "topLevelItem" [intT] (ptrT $ objT c_QTreeWidgetItem)
  , just $ mkConstMethod "topLevelItemCount" [] intT
  -- TODO add more methods
  ]

c_QTreeWidgetItem :: Class
c_QTreeWidgetItem =
  addReqIncludes [includeStd "QTreeWidgetItem"] $
  classSetEntityPrefix "" $
  makeClass (ident "QTreeWidgetItem") Nothing [] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithType" [intT]
  , just $ mkCtor "newWithStrings" [objT c_QStringList]
  , just $ mkCtor "newWithStringsAndType" [objT c_QStringList, intT]
  , just $ mkCtor "newWithParentTree" [ptrT $ objT c_QTreeWidget]
  , just $ mkCtor "newWithParentTreeAndType" [ptrT $ objT c_QTreeWidget, intT]
  , just $ mkCtor "newWithParentTreeAndStrings" [ptrT $ objT c_QTreeWidget, objT c_QStringList]
  , just $ mkCtor "newWithParentTreeAndStringsAndType"
    [ptrT $ objT c_QTreeWidget, objT c_QStringList, intT]
  , just $ mkCtor "newWithParentItem" [ptrT $ objT c_QTreeWidgetItem]
  , just $ mkCtor "newWithParentItemAndType" [ptrT $ objT c_QTreeWidgetItem, intT]
  , just $ mkCtor "newWithParentItemAndStrings" [ptrT $ objT c_QTreeWidgetItem, objT c_QStringList]
  , just $ mkCtor "newWithParentItemAndStringsAndType"
    [ptrT $ objT c_QTreeWidgetItem, objT c_QStringList, intT]
  , just $ mkConstMethod "child" [intT] (ptrT $ objT c_QTreeWidgetItem)
  , just $ mkConstMethod "childCount" [] intT
  , just $ mkConstMethod "columnCount" [] intT
  , test (qtVersion >= [4, 2]) $ mkConstMethod "isHidden" [] boolT
  , just $ mkConstMethod "parent" [] (ptrT $ objT c_QTreeWidgetItem)
  , test (qtVersion >= [4, 2]) $ mkConstMethod "setHidden" [boolT] voidT
  , just $ mkMethod "setIcon" [intT, objT c_QIcon] voidT
  , just $ mkMethod "setText" [intT, objT c_QString] voidT
  , just $ mkConstMethod "text" [intT] (objT c_QString)
  , just $ mkConstMethod' "type" "getType" [] intT
  -- TODO add more methods
  ]

signals :: [Signal]
signals =
  [ -- TODO add signals
  ]

itemSignals :: [Signal]
itemSignals =
  [ -- TODO add signals
  ]
