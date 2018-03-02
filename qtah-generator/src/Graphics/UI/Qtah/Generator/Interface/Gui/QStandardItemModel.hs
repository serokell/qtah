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

module Graphics.UI.Qtah.Generator.Interface.Gui.QStandardItemModel (
  aModule,
  itemModule,
  itemListModule,
  c_QStandardItemModel,
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
  mkMethod',
  mkProp,
  )
import Foreign.Hoppy.Generator.Types (
  bitspaceT, intT, objT, ptrT, voidT,
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QAbstractItemModel (
  c_QAbstractItemModel,
  )
import Graphics.UI.Qtah.Generator.Interface.Core.QList (
  Contents, c_QList, instantiate,
  )
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QVariant (c_QVariant)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (bs_Alignment)
import Graphics.UI.Qtah.Generator.Interface.Gui.QFont (c_QFont)
import Graphics.UI.Qtah.Generator.Interface.Gui.QIcon (c_QIcon)
import Graphics.UI.Qtah.Generator.Module (
  AModule (AQtModule), makeQtModule, makeQtModuleWithMinVersion,
  )
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule :: AModule
aModule =
  AQtModule $
  makeQtModule ["Gui", "QStandardItemModel"]
  [QtExport $ ExportClass c_QStandardItemModel]

itemModule :: AModule
itemModule =
  AQtModule $
  makeQtModuleWithMinVersion
    ["Gui", "QStandardItem"]
    [4, 2]
    [QtExport $ ExportClass c_QStandardItem]

itemListModule :: AModule
itemListModule =
  AQtModule $
  makeQtModuleWithMinVersion
    ["Core", "QList", "QStandardItem"]
    [4, 2]
    [QtExport $ ExportClass c_QListQStandardItem]

c_QStandardItemModel :: Class
c_QStandardItemModel =
  addReqIncludes [includeStd "QStandardItemModel"] $
  classSetEntityPrefix "" $
  makeClass (ident "QStandardItemModel") Nothing [c_QAbstractItemModel] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QObject]
  , just $ mkCtor "newWithRowsAndColumns" [intT, intT]
  , just $
    mkCtor "newWithRowsAndColumnsAndParent" [intT, intT, ptrT $ objT c_QObject]
  , test (qtVersion >= [4, 2]) $
    mkMethod' "appendRow" "appendRowItems" [objT c_QListQStandardItem] voidT
  , test (qtVersion >= [4, 2]) $
    mkMethod' "appendRow" "appendRowItem" [ptrT $ objT c_QStandardItem] voidT
  ]

c_QStandardItem :: Class
c_QStandardItem =
  addReqIncludes [includeStd "QStandardItem"] $
  classSetEntityPrefix "" $
  makeClass (ident "QStandardItem") Nothing [] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithText" [objT c_QString]
  , just $ mkCtor "newWithIconAndText" [objT c_QIcon, objT c_QString]
  , just $ mkCtor "newWithRows" [intT]
  , just $ mkCtor "newWithRowsAndColumns" [intT, intT]
  , just $
    mkMethod' "appendRow" "appendRowItems" [objT c_QListQStandardItem] voidT
  , just $
    mkMethod' "appendRow" "appendRowItem" [ptrT $ objT c_QStandardItem] voidT
  , just $ mkConstMethod' "data" "getData" [] (objT c_QVariant)
  , just $ mkConstMethod' "data" "getDataWithRole" [intT] (objT c_QVariant)
  , just $ mkConstMethod "font" [] (objT c_QFont)
  , just $ mkConstMethod "model" [] (ptrT $ objT c_QAbstractItemModel)
  , just $ mkMethod "setData" [objT c_QVariant] voidT
  , just $ mkMethod' "setData" "setDataWithRole" [objT c_QVariant, intT] voidT
  , just $ mkMethod "setFont" [objT c_QFont] voidT
  , just $ mkProp "text" $ objT c_QString
  , just $ mkProp "textAlignment" $ bitspaceT bs_Alignment
  -- TODO other methods
  ]

c_QListQStandardItem :: Class
c_QListQStandardItem = c_QList contents_QStandardItem

contents_QStandardItem :: Contents
contents_QStandardItem =
  instantiate "QListQStandardItem" (ptrT $ objT c_QStandardItem) mempty
