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

module Graphics.UI.Qtah.Internal.Interface.Widgets.QTabWidget (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass, ExportEnum),
  Type (TBool, TEnum, TInt, TObj, TPtr, TVoid),
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
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Internal.Flags (qtVersion)
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (e_Corner, e_TextElideMode)
import Graphics.UI.Qtah.Internal.Interface.Listener (c_ListenerInt)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

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
  , mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  ] $
  collect
  [ just $ mkMethod "addTab" [TPtr $ TObj c_QWidget, TObj c_QString] TInt
    -- TODO addTabWithIcon
  , just $ mkMethod "clear" [] TVoid
  , just $ mkConstMethod "cornerWidget" [TEnum e_Corner] $ TPtr $ TObj c_QWidget
  , just $ mkConstMethod "count" [] TInt
  , just $ mkConstMethod "indexOf" [TPtr $ TObj c_QWidget] TInt
  , just $ mkMethod "insertTab" [TInt, TPtr $ TObj c_QWidget, TObj c_QString] TInt
    -- TODO insertTabWithIcon
  , just $ mkConstMethod "isTabEnabled" [TInt] TBool
  , just $ mkMethod "removeTab" [TInt] TVoid
  , just $ mkMethod "setCornerWidget" [TPtr $ TObj c_QWidget, TEnum e_Corner] TVoid
  , just $ mkMethod "setTabEnabled" [TInt, TBool] TVoid
    -- TODO setTabIcon
  , just $ mkMethod "setTabText" [TInt, TObj c_QString] TVoid
  , just $ mkMethod "setTabToolTip" [TInt, TObj c_QString] TVoid
  , test (qtVersion >= [4, 1]) $ mkMethod "setTabWhatsThis" [TInt, TObj c_QString] TVoid
    -- TODO tabBar
    -- TODO tabIcon
  , just $ mkConstMethod "tabText" [TInt] $ TObj c_QString
  , just $ mkConstMethod "tabToolTip" [TInt] $ TObj c_QString
  , just $ mkConstMethod "tabWhatsThis" [TInt] $ TObj c_QString
  , just $ mkConstMethod "widget" [TInt] $ TPtr $ TObj c_QWidget
  ] ++
  (mkProps . collect)
  [ just $ mkProp "currentIndex" TInt
  , just $ mkProp "currentWidget" $ TPtr $ TObj c_QWidget
  , test (qtVersion >= [4, 5]) $ mkProp "documentMode" TBool
  , test (qtVersion >= [4, 2]) $ mkProp "elideMode" $ TEnum e_TextElideMode
  , test (qtVersion >= [4, 2]) $ mkProp "iconSize" $ TObj c_QSize
  , test (qtVersion >= [4, 5]) $ mkBoolIsProp "movable"
  , test (qtVersion >= [5, 4]) $ mkProp "tabBarAutoHide" TBool
  , just $ mkProp "tabPosition" $ TEnum e_TabPosition
  , just $ mkProp "tabShape" $ TEnum e_TabShape
  , test (qtVersion >= [4, 5]) $ mkProp "tabsClosable" TBool
  , test (qtVersion >= [4, 2]) $ mkProp "usesScrollButtons" TBool
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
