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
  mkMethod',
  )
import Foreign.Hoppy.Generator.Types (boolT, enumT, objT, refT, voidT, ptrT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QList (c_QList, Contents, instantiate)
import Graphics.UI.Qtah.Generator.Interface.Core.QAbstractItemModel (c_QAbstractItemModel)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule, QtModule, makeQtModuleWithMinVersion)
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
  [ test (qtVersion >= [4, 2]) $
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
  [
  ]

c_QListQStandardItem :: Class
c_QListQStandardItem = c_QList contents_QStandardItem

contents_QStandardItem :: Contents
contents_QStandardItem =
  instantiate "QListQStandardItem" (ptrT $ objT c_QStandardItem) mempty
