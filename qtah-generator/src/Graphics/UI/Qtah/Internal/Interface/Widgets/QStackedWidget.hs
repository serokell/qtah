-- This file is part of Qtah.
--
-- Copyright 2016 Bryan Gardiner <bog@khumba.net>
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

module Graphics.UI.Qtah.Internal.Interface.Widgets.QStackedWidget (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  Type (TInt, TObj, TPtr, TVoid),
  addReqIncludes,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkProp,
  mkProps,
  )
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Listener (c_ListenerInt)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QFrame (c_QFrame)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QStackedWidget"] $
  QtExport (ExportClass c_QStackedWidget) :
  map QtExportSignal signals

c_QStackedWidget =
  addReqIncludes [includeStd "QStackedWidget"] $
  makeClass (ident "QStackedWidget") Nothing [c_QFrame]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  ] $
  [ mkMethod "addWidget" [TPtr $ TObj c_QWidget] TInt
  , mkConstMethod "count" [] TInt
  , mkConstMethod "indexOf" [TPtr $ TObj c_QWidget] TInt
  , mkMethod "insertWidget" [TInt, TPtr $ TObj c_QWidget] TInt
  , mkMethod "removeWidget" [TPtr $ TObj c_QWidget] TVoid
  , mkConstMethod "widget" [TInt] $ TPtr $ TObj c_QWidget
  ] ++
  mkProps
  [ mkProp "currentIndex" TInt
  , mkProp "currentWidget" $ TPtr $ TObj c_QWidget
  ]

signals =
  [ makeSignal c_QStackedWidget "currentChanged" c_ListenerInt
  , makeSignal c_QStackedWidget "widgetRemoved" c_ListenerInt
  ]
