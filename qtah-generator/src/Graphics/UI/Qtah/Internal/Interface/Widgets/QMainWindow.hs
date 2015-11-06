-- This file is part of Qtah.
--
-- Copyright 2015 Bryan Gardiner <bog@khumba.net>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License version 3
-- as published by the Free Software Foundation.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Graphics.UI.Qtah.Internal.Interface.Widgets.QMainWindow (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  Type (TBool, TObj, TPtr),
  addReqIncludes,
  ident,
  includeStd,
  makeClass,
  mkBoolIsProp,
  mkCtor,
  mkMethod,
  mkProp,
  mkProps,
  )
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Internal.Interface.Listener (c_ListenerQSize)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QMenu (c_QMenu)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QMenuBar (c_QMenuBar)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QStatusBar (c_QStatusBar)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QMainWindow"] $
  QtExport (ExportClass c_QMainWindow) :
  map QtExportSignal signals

c_QMainWindow =
  addReqIncludes [includeStd "QMainWindow"] $
  makeClass (ident "QMainWindow") Nothing [c_QWidget]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
    -- TODO Ctor with Qt::WindowFlags.
  ] $
  [ -- TODO addDockWidget
    -- TODO addToolBar
    -- TODO addToolBarBreak
    -- TODO corner
    mkMethod "createPopupMenu" [] $ TPtr $ TObj c_QMenu
    -- TODO dockWidgetArea
    -- TODO insertToolBar
    -- TODO insertToolBarBreak
    -- TODO removeDockWidget
    -- TODO restoreState
    -- TODO saveState
    -- TODO setCorner
    -- TODO setTabPosition
    -- TODO setTabShape
    -- TODO splitDockWidget
    -- TODO tabifiedDockWidgets
    -- TODO tabifyDockWidget
    -- TODO tabPosition
    -- TODO tabShape
    -- TODO toolBarArea
    -- TODO toolBarBreak
  ] ++
  mkProps
  [ mkBoolIsProp "animated"
  , mkProp "centralWidget" $ TPtr $ TObj c_QWidget
  , mkBoolIsProp "dockNestingEnabled"
    -- TODO dockOptions
  , mkProp "documentMode" TBool
  , mkProp "iconSize" $ TObj c_QSize
  , mkProp "menuBar" $ TPtr $ TObj c_QMenuBar
  , mkProp "menuWidget" $ TPtr $ TObj c_QWidget
  , mkProp "statusBar" $ TPtr $ TObj c_QStatusBar
    -- TODO tabShape
    -- TODO toolButtonStyle
  , mkProp "unifiedTitleAndToolBarOnMac" TBool
  ]

signals =
  [ makeSignal c_QMainWindow "iconSizeChanged" c_ListenerQSize
    -- TODO toolButtonStyleChanged
  ]
