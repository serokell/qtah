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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QTreeView (
  aModule,
  c_QTreeView,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Class,
  Export (ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkCtor,
  mkBoolIsProp,
  mkMethod,
  )
import Foreign.Hoppy.Generator.Types (intT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractItemView (c_QAbstractItemView)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule :: AModule
aModule =
  AQtModule $
  makeQtModule ["Widgets", "QTreeView"] $
  QtExport (ExportClass c_QTreeView) :
  map QtExportSignal signals

c_QTreeView :: Class
c_QTreeView =
  addReqIncludes [includeStd "QTreeView"] $
  classSetEntityPrefix "" $
  makeClass (ident "QTreeView") Nothing [c_QAbstractItemView] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkBoolIsProp "headerHidden"
  , just $ mkMethod "resizeColumnToContents" [intT] voidT
  , test (qtVersion >= [4, 2]) $ mkBoolIsProp "sortingEnabled"
  -- TODO add more methods
  ]

signals :: [Signal]
signals =
  [ -- TODO add signals
  ]
