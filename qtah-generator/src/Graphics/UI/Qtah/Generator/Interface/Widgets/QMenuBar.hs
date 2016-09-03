-- This file is part of Qtah.
--
-- Copyright 2015-2016 Bryan Gardiner <bog@khumba.net>
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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QMenuBar (
  aModule,
  c_QMenuBar,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkBoolIsProp,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkMethod',
  mkProp,
  mkProps,
  )
import Foreign.Hoppy.Generator.Types (enumT, objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Flag (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (wsWince)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_Corner)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (c_ListenerPtrQAction)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAction (c_QAction)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QMenu (c_QMenu)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QMenuBar"] $
  QtExport (ExportClass c_QMenuBar) :
  map QtExportSignal signals

c_QMenuBar =
  addReqIncludes [includeStd "QMenuBar"] $
  classSetEntityPrefix "" $
  makeClass (ident "QMenuBar") Nothing
  [ c_QWidget ]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  ] $
  [ mkConstMethod "actionAt" [objT c_QPoint] $ ptrT $ objT c_QAction
  , mkConstMethod "actionGeometry" [ptrT $ objT c_QAction] $ objT c_QRect
  , mkMethod' "addAction" "addAction" [ptrT $ objT c_QAction] voidT
  , mkMethod' "addAction" "addNewAction" [objT c_QString] $ ptrT $ objT c_QAction
    -- TODO addNewActionWithIcon and connecting forms
  , mkMethod' "addMenu" "addMenu" [ptrT $ objT c_QMenu] $ ptrT $ objT c_QAction
  , mkMethod' "addMenu" "addNewMenu" [objT c_QString] $ ptrT $ objT c_QMenu
    -- TODO addNewMenuWithIcon
  , mkMethod "addSeparator" [] $ ptrT $ objT c_QAction
  , mkMethod "clear" [] voidT
  , mkConstMethod "cornerWidget" [enumT e_Corner] $ ptrT $ objT c_QWidget
  , mkMethod "insertMenu" [ptrT $ objT c_QAction, ptrT $ objT c_QMenu] $ ptrT $ objT c_QAction
  , mkMethod "insertSeparator" [ptrT $ objT c_QAction] $ ptrT $ objT c_QAction
  , mkMethod "setCornerWidget" [ptrT $ objT c_QWidget, enumT e_Corner] voidT
  ] ++
  (mkProps . collect)
  [ just $ mkProp "activeAction" $ ptrT $ objT c_QAction
  , test wsWince $ mkProp "defaultAction" $ ptrT $ objT c_QAction
  , just $ mkBoolIsProp "defaultUp"
  , just $ mkBoolIsProp "nativeMenuBar"
  ]

signals =
  [ makeSignal c_QMenuBar "hovered" c_ListenerPtrQAction
  , makeSignal c_QMenuBar "triggered" c_ListenerPtrQAction
  ]
