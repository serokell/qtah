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

module Graphics.UI.Qtah.Internal.Interface.Widgets.QMenu (
  aModule,
  c_QMenu,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  Type (TBool, TObj, TPtr, TVoid),
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
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Internal.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Listener (c_Listener, c_ListenerPtrQAction)
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.Widgets.QAction (c_QAction)
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QMenu"] $
  QtExport (ExportClass c_QMenu) :
  map QtExportSignal signals

c_QMenu =
  addReqIncludes [includeStd "QMenu"] $
  makeClass (ident "QMenu") Nothing
  [ c_QWidget ]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  , mkCtor "newWithTitle" [TObj c_QString]
  , mkCtor "newWithTitleAndParent" [TObj c_QString, TPtr $ TObj c_QWidget]
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
  , mkMethod' "exec" "exec" [] $ TPtr $ TObj c_QAction
  , mkMethod' "exec" "execAt" [TObj c_QPoint, TPtr $ TObj c_QAction] $ TPtr $ TObj c_QAction
    -- TODO Static exec
  , mkMethod "hideTearOffMenu" [] TVoid
  , mkMethod "insertMenu" [TPtr $ TObj c_QAction, TPtr $ TObj c_QMenu] $ TPtr $ TObj c_QAction
  , mkMethod "insertSeparator" [TPtr $ TObj c_QAction] $ TPtr $ TObj c_QAction
  , mkConstMethod "isEmpty" [] TBool
  , mkConstMethod "isTearOffMenuVisible" [] TBool
  , mkConstMethod "menuAction" [] $ TPtr $ TObj c_QAction
  , mkMethod' "popup" "popup" [TObj c_QPoint] TVoid
  , mkMethod' "popup" "popupAction" [TObj c_QPoint, TPtr $ TObj c_QAction] TVoid
    -- TODO setIcon
  ] ++
  mkProps
  [ mkProp "activeAction" $ TPtr $ TObj c_QAction
  , mkProp "defaultAction" $ TPtr $ TObj c_QAction
    -- TODO icon
  , mkProp "separatorsCollapsible" TBool
  , mkBoolIsProp "tearOffEnabled"
  , mkProp "title" $ TObj c_QString
  ]

signals =
  [ makeSignal c_QMenu "aboutToHide" c_Listener
  , makeSignal c_QMenu "aboutToShow" c_Listener
  , makeSignal c_QMenu "hovered" c_ListenerPtrQAction
  , makeSignal c_QMenu "triggered" c_ListenerPtrQAction
  ]
