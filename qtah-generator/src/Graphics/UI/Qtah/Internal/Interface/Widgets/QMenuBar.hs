-- This file is part of Qtah.
--
-- Copyright 2015 Bryan Gardiner <bog@khumba.net>
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

module Graphics.UI.Qtah.Internal.Interface.Widgets.QMenuBar (
  aModule,
  c_QMenuBar,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  Type (TEnum, TObj, TPtr, TVoid),
  addReqIncludes,
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
import Graphics.UI.Qtah.Internal.Flag (collect, just, test)
import Graphics.UI.Qtah.Internal.Flags (wsWince)
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Internal.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (e_Corner)
import Graphics.UI.Qtah.Internal.Interface.Listener (c_ListenerPtrQAction)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QAction (c_QAction)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QMenu (c_QMenu)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QMenuBar"] $
  QtExport (ExportClass c_QMenuBar) :
  map QtExportSignal signals

c_QMenuBar =
  addReqIncludes [includeStd "QMenuBar"] $
  makeClass (ident "QMenuBar") Nothing
  [ c_QWidget ]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  ] $
  [ mkConstMethod "actionAt" [TObj c_QPoint] $ TPtr $ TObj c_QAction
  , mkConstMethod "actionGeometry" [TPtr $ TObj c_QAction] $ TObj c_QRect
  , mkMethod' "addAction" "addAction" [TPtr $ TObj c_QAction] TVoid
  , mkMethod' "addAction" "addNewAction" [TObj c_QString] $ TPtr $ TObj c_QAction
    -- TODO addNewActionWithIcon and connecting forms
  , mkMethod' "addMenu" "addMenu" [TPtr $ TObj c_QMenu] $ TPtr $ TObj c_QAction
  , mkMethod' "addMenu" "addNewMenu" [TObj c_QString] $ TPtr $ TObj c_QMenu
    -- TODO addNewMenuWithIcon
  , mkMethod "addSeparator" [] $ TPtr $ TObj c_QAction
  , mkMethod "clear" [] TVoid
  , mkConstMethod "cornerWidget" [TEnum e_Corner] $ TPtr $ TObj c_QWidget
  , mkMethod "insertMenu" [TPtr $ TObj c_QAction, TPtr $ TObj c_QMenu] $ TPtr $ TObj c_QAction
  , mkMethod "insertSeparator" [TPtr $ TObj c_QAction] $ TPtr $ TObj c_QAction
  , mkMethod "setCornerWidget" [TPtr $ TObj c_QWidget, TEnum e_Corner] TVoid
  ] ++
  (mkProps . collect)
  [ just $ mkProp "activeAction" $ TPtr $ TObj c_QAction
  , test wsWince $ mkProp "defaultAction" $ TPtr $ TObj c_QAction
  , just $ mkBoolIsProp "defaultUp"
  , just $ mkBoolIsProp "nativeMenuBar"
  ]

signals =
  [ makeSignal c_QMenuBar "hovered" c_ListenerPtrQAction
  , makeSignal c_QMenuBar "triggered" c_ListenerPtrQAction
  ]
