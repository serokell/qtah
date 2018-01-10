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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QStackedWidget (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkProp,
  )
import Foreign.Hoppy.Generator.Types (intT, objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (c_ListenerInt)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QFrame (c_QFrame)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QStackedWidget"] $
  QtExport (ExportClass c_QStackedWidget) :
  map QtExportSignal signals

c_QStackedWidget =
  addReqIncludes [includeStd "QStackedWidget"] $
  classSetEntityPrefix "" $
  makeClass (ident "QStackedWidget") Nothing [c_QFrame]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , mkMethod "addWidget" [ptrT $ objT c_QWidget] intT
  , mkConstMethod "count" [] intT
  , mkProp "currentIndex" intT
  , mkProp "currentWidget" $ ptrT $ objT c_QWidget
  , mkConstMethod "indexOf" [ptrT $ objT c_QWidget] intT
  , mkMethod "insertWidget" [intT, ptrT $ objT c_QWidget] intT
  , mkMethod "removeWidget" [ptrT $ objT c_QWidget] voidT
  , mkConstMethod "widget" [intT] $ ptrT $ objT c_QWidget
  ]

signals =
  [ makeSignal c_QStackedWidget "currentChanged" c_ListenerInt
  , makeSignal c_QStackedWidget "widgetRemoved" c_ListenerInt
  ]
