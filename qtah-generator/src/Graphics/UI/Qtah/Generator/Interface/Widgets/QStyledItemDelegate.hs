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
  Export (ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkCtor,
  )
import Foreign.Hoppy.Generator.Types (objT, ptrT)
-- import Graphics.UI.Qtah.Generator.Interface.Core.QLocale (c_QLocale)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractItemDelegate (c_QAbstractItemDelegate)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

minVersion = [4, 4]

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Widgets", "QStyledItemDelegate"] minVersion
  [ QtExport $ ExportClass c_QStyledItemDelegate ]

c_QStyledItemDelegate =
  addReqIncludes [includeStd "QStyledItemDelegate"] $
  classSetEntityPrefix "" $
  makeClass (ident "QStyledItemDelegate") Nothing [c_QAbstractItemDelegate] $
  [
  -- Public Functions
    mkCtor "new" []
  , mkCtor "newWithParent" [ptrT $ objT c_QObject]
  -- TODO mkConstMethod
  --     "displayText" [objT c_QVariant, objT c_QLocale] (objT c_QString)
  -- TODO QItemEditorFactory * itemEditorFactory() const
  -- TODO void setItemEditorFactory(QItemEditorFactory *factory)
  ]
