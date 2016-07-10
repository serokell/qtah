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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QTabWidget (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass, ExportEnum),
  addReqIncludes,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkBoolIsProp,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkProp,
  mkProps,
  )
import Foreign.Hoppy.Generator.Types (boolT, enumT, intT, objT, ptrT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_Corner, e_TextElideMode)
import Graphics.UI.Qtah.Generator.Interface.Listener (c_ListenerInt)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QTabWidget"] $
  QtExport (ExportClass c_QTabWidget) :
  map QtExportSignal signals ++
  [ QtExport $ ExportEnum e_TabPosition
  , QtExport $ ExportEnum e_TabShape
  ]

c_QTabWidget =
  addReqIncludes [includeStd "QTabWidget"] $
  makeClass (ident "QTabWidget") Nothing [c_QWidget]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  ] $
  collect
  [ just $ mkMethod "addTab" [ptrT $ objT c_QWidget, objT c_QString] intT
    -- TODO addTabWithIcon
  , just $ mkMethod "clear" [] voidT
  , just $ mkConstMethod "cornerWidget" [enumT e_Corner] $ ptrT $ objT c_QWidget
  , just $ mkConstMethod "count" [] intT
  , just $ mkConstMethod "indexOf" [ptrT $ objT c_QWidget] intT
  , just $ mkMethod "insertTab" [intT, ptrT $ objT c_QWidget, objT c_QString] intT
    -- TODO insertTabWithIcon
  , just $ mkConstMethod "isTabEnabled" [intT] boolT
  , just $ mkMethod "removeTab" [intT] voidT
  , just $ mkMethod "setCornerWidget" [ptrT $ objT c_QWidget, enumT e_Corner] voidT
  , just $ mkMethod "setTabEnabled" [intT, boolT] voidT
    -- TODO setTabIcon
  , just $ mkMethod "setTabText" [intT, objT c_QString] voidT
  , just $ mkMethod "setTabToolTip" [intT, objT c_QString] voidT
  , test (qtVersion >= [4, 1]) $ mkMethod "setTabWhatsThis" [intT, objT c_QString] voidT
    -- TODO tabBar
    -- TODO tabIcon
  , just $ mkConstMethod "tabText" [intT] $ objT c_QString
  , just $ mkConstMethod "tabToolTip" [intT] $ objT c_QString
  , just $ mkConstMethod "tabWhatsThis" [intT] $ objT c_QString
  , just $ mkConstMethod "widget" [intT] $ ptrT $ objT c_QWidget
  ] ++
  (mkProps . collect)
  [ just $ mkProp "currentIndex" intT
  , just $ mkProp "currentWidget" $ ptrT $ objT c_QWidget
  , test (qtVersion >= [4, 5]) $ mkProp "documentMode" boolT
  , test (qtVersion >= [4, 2]) $ mkProp "elideMode" $ enumT e_TextElideMode
  , test (qtVersion >= [4, 2]) $ mkProp "iconSize" $ objT c_QSize
  , test (qtVersion >= [4, 5]) $ mkBoolIsProp "movable"
  , test (qtVersion >= [5, 4]) $ mkProp "tabBarAutoHide" boolT
  , just $ mkProp "tabPosition" $ enumT e_TabPosition
  , just $ mkProp "tabShape" $ enumT e_TabShape
  , test (qtVersion >= [4, 5]) $ mkProp "tabsClosable" boolT
  , test (qtVersion >= [4, 2]) $ mkProp "usesScrollButtons" boolT
  ]

e_TabPosition =
  makeQtEnum (ident1 "QTabWidget" "TabPosition") [includeStd "QTabWidget"]
  [ (0, ["north"])
  , (1, ["south"])
  , (2, ["west"])
  , (3, ["east"])
  ]

e_TabShape =
  makeQtEnum (ident1 "QTabWidget" "TabShape") [includeStd "QTabWidget"]
  [ (0, ["rounded"])
  , (1, ["triangular"])
  ]

signals =
  [ makeSignal c_QTabWidget "currentChanged" c_ListenerInt
  , makeSignal c_QTabWidget "tabBarClicked" c_ListenerInt
  , makeSignal c_QTabWidget "tabBarDoubleClicked" c_ListenerInt
  , makeSignal c_QTabWidget "tabCloseRequested" c_ListenerInt
  ]
