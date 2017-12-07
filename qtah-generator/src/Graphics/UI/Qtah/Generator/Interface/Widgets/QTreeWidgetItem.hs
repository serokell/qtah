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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QTreeWidgetItem (
  aModule,
  c_QTreeWidgetItem,
  ) where

import Foreign.Hoppy.Generator.Spec (
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
import Foreign.Hoppy.Generator.Types (boolT, enumT, intT, objT, ptrT, voidT, refT, constT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QStringList (c_QStringList)
import Graphics.UI.Qtah.Generator.Interface.Gui.QIcon (c_QIcon)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QTreeWidgetItem"] $
  QtExport (ExportClass c_QTreeWidgetItem) :
  map QtExportSignal signals

c_QTreeWidgetItem =
  addReqIncludes [includeStd "QTreeWidgetItem"] $
  classSetEntityPrefix "" $
  makeClass (ident "QTreeWidgetItem") Nothing [] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithType" [intT]
  , just $ mkCtor "newWithStrings" [objT c_QStringList]
  , just $ mkCtor "newWithStringsAndType" [objT c_QStringList, intT]
  , just $ mkCtor "newWithParentItem" [ptrT $ objT c_QTreeWidgetItem]
  , just $ mkCtor "newWithParentItemAndType" [ptrT $ objT c_QTreeWidgetItem, intT]
  , just $ mkCtor "newWithParentItemAndStrings" [ptrT $ objT c_QTreeWidgetItem, objT c_QStringList]
  , just $ mkCtor "newWithParentItemAndStringsAndType" [ptrT $ objT c_QTreeWidgetItem, objT c_QStringList, intT]
  , just $ mkMethod "setIcon" [intT, objT c_QIcon] voidT
  , just $ mkMethod "setText" [intT, objT c_QString] voidT
  , just $ mkConstMethod' "type" "getType" [] intT
  ]

signals =
  [
  ]
