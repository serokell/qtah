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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QMenu (
  aModule,
  c_QMenu,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
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
import Foreign.Hoppy.Generator.Types (boolT, objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Listener (c_Listener, c_ListenerPtrQAction)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QAction (c_QAction)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

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
  , mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , mkCtor "newWithTitle" [objT c_QString]
  , mkCtor "newWithTitleAndParent" [objT c_QString, ptrT $ objT c_QWidget]
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
  , mkMethod' "exec" "exec" [] $ ptrT $ objT c_QAction
  , mkMethod' "exec" "execAt" [objT c_QPoint, ptrT $ objT c_QAction] $ ptrT $ objT c_QAction
    -- TODO Static exec
  , mkMethod "hideTearOffMenu" [] voidT
  , mkMethod "insertMenu" [ptrT $ objT c_QAction, ptrT $ objT c_QMenu] $ ptrT $ objT c_QAction
  , mkMethod "insertSeparator" [ptrT $ objT c_QAction] $ ptrT $ objT c_QAction
  , mkConstMethod "isEmpty" [] boolT
  , mkConstMethod "isTearOffMenuVisible" [] boolT
  , mkConstMethod "menuAction" [] $ ptrT $ objT c_QAction
  , mkMethod' "popup" "popup" [objT c_QPoint] voidT
  , mkMethod' "popup" "popupAction" [objT c_QPoint, ptrT $ objT c_QAction] voidT
    -- TODO setIcon
  ] ++
  mkProps
  [ mkProp "activeAction" $ ptrT $ objT c_QAction
  , mkProp "defaultAction" $ ptrT $ objT c_QAction
    -- TODO icon
  , mkProp "separatorsCollapsible" boolT
  , mkBoolIsProp "tearOffEnabled"
  , mkProp "title" $ objT c_QString
  ]

signals =
  [ makeSignal c_QMenu "aboutToHide" c_Listener
  , makeSignal c_QMenu "aboutToShow" c_Listener
  , makeSignal c_QMenu "hovered" c_ListenerPtrQAction
  , makeSignal c_QMenu "triggered" c_ListenerPtrQAction
  ]
