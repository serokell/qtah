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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QToolBar (
  aModule,
  c_QToolBar,
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
  mkMethod,
  )
import Foreign.Hoppy.Generator.Types (objT, ptrT)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAction (c_QAction)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule :: AModule
aModule =
  AQtModule $
  makeQtModule ["Widgets", "QToolBar"] $
  QtExport (ExportClass c_QToolBar) :
  map QtExportSignal signals

c_QToolBar :: Class
c_QToolBar =
  addReqIncludes [includeStd "QToolBar"] $
  classSetEntityPrefix "" $
  makeClass (ident "QToolBar") Nothing [c_QWidget]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , mkCtor "newWithTitle" [objT c_QString]
  , mkCtor "newWithTitleAndParent" [objT c_QString, ptrT $ objT c_QWidget]
  , mkMethod "addWidget" [ptrT $ objT c_QWidget] (ptrT $ objT c_QAction)
  , mkMethod "insertWidget" [ptrT $ objT c_QAction, ptrT $ objT c_QWidget] (ptrT $ objT c_QAction)
  -- TODO add methods
  ]

signals :: [Signal]
signals =
  [ -- TODO add signals
  ]
