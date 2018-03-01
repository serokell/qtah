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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QMainWindow (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Class,
  Export (ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkBoolIsProp,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkMethod',
  mkProp,
  )
import Foreign.Hoppy.Generator.Types (boolT, intT, objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (c_ListenerQSize)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QMenu (c_QMenu)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QMenuBar (c_QMenuBar)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QStatusBar (c_QStatusBar)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QToolBar (c_QToolBar)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule :: AModule
aModule =
  AQtModule $
  makeQtModule ["Widgets", "QMainWindow"] $
  QtExport (ExportClass c_QMainWindow) :
  map QtExportSignal signals

c_QMainWindow :: Class
c_QMainWindow =
  addReqIncludes [includeStd "QMainWindow"] $
  classSetEntityPrefix "" $
  makeClass (ident "QMainWindow") Nothing [c_QWidget]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [ptrT $ objT c_QWidget]
    -- TODO Ctor with Qt::WindowFlags.
    -- TODO addDockWidget
    -- TODO mkMethod' "addToolBar" "addToolBarWithArea" [e_ToolBarArea, ptrT $ objT c_QToolBar]
    --      voidT
  , mkMethod "addToolBar" [ptrT $ objT c_QToolBar] voidT
  , mkMethod' "addToolBar" "addToolBarWithTitle" [objT c_QString] (ptrT $ objT c_QToolBar)
    -- TODO addToolBarBreak
  , mkBoolIsProp "animated"
  , mkProp "centralWidget" $ ptrT $ objT c_QWidget
    -- TODO corner
  , mkMethod "createPopupMenu" [] $ ptrT $ objT c_QMenu
  , mkBoolIsProp "dockNestingEnabled"
    -- TODO dockOptions
    -- TODO dockWidgetArea
  , mkProp "documentMode" boolT
  , mkProp "iconSize" $ objT c_QSize
    -- TODO insertToolBar
    -- TODO insertToolBarBreak
  , mkProp "menuBar" $ ptrT $ objT c_QMenuBar
  , mkProp "menuWidget" $ ptrT $ objT c_QWidget
    -- TODO removeDockWidget
  , mkMethod "restoreState" [objT c_QByteArray] boolT
  , mkMethod' "restoreState" "restoreStateWithVersion" [objT c_QByteArray, intT] boolT
  , mkConstMethod "saveState" [] (objT c_QByteArray)
  , mkConstMethod' "saveState" "saveStateWithVersion" [intT] (objT c_QByteArray)
    -- TODO setCorner
    -- TODO setTabPosition
    -- TODO setTabShape
    -- TODO splitDockWidget
  , mkProp "statusBar" $ ptrT $ objT c_QStatusBar
    -- TODO tabifiedDockWidgets
    -- TODO tabifyDockWidget
    -- TODO tabPosition
    -- TODO tabShape
    -- TODO toolBarArea
    -- TODO toolBarBreak
    -- TODO toolButtonStyle
  , mkProp "unifiedTitleAndToolBarOnMac" boolT
  ]

signals :: [Signal]
signals =
  [ makeSignal c_QMainWindow "iconSizeChanged" c_ListenerQSize
    -- TODO toolButtonStyleChanged
  ]
