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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QDockWidget (
  aModule,
  c_QDockWidget,
  e_DockWidgetFeature,
  bs_DockWidgetFeatures,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportBitspace, ExportClass, ExportEnum),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkBoolIsProp,
  mkConstMethod,
  mkCtor,
  mkProp,
  )
import Foreign.Hoppy.Generator.Types (bitspaceT, boolT, enumT, objT, ptrT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (
  e_DockWidgetArea,
  bs_DockWidgetAreas,
  )
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  c_ListenerBool,
  c_ListenerDockWidgetArea,
  c_ListenerDockWidgetAreas,
  c_ListenerQDockWidgetFeatures,
  )
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAction (c_QAction)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QDockWidget"] $
  map QtExport
  [ ExportClass c_QDockWidget
  , ExportEnum e_DockWidgetFeature
  , ExportBitspace bs_DockWidgetFeatures
  ] ++
  map QtExportSignal signals

c_QDockWidget =
  addReqIncludes [includeStd "QDockWidget"] $
  classSetEntityPrefix "" $
  makeClass (ident "QDockWidget") Nothing [c_QWidget] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , just $ mkCtor "newWithText" [objT c_QString]
  , just $ mkCtor "newWithTextAndParent" [objT c_QString, ptrT $ objT c_QWidget]
    -- TODO Ctor with Qt::WindowFlags.
  , just $ mkProp "allowedAreas" $ bitspaceT bs_DockWidgetAreas
  , just $ mkConstMethod "isAreaAllowed" [enumT e_DockWidgetArea] $ boolT
  , just $ mkProp "features" $ bitspaceT bs_DockWidgetFeatures
  , just $ mkBoolIsProp "floating"
  , test (qtVersion >= [4, 3]) $ mkProp "titleBarWidget" $ ptrT $ objT c_QWidget
  , just $ mkConstMethod "toggleViewAction" [] $ ptrT $ objT c_QAction
  , just $ mkProp "widget" $ ptrT $ objT c_QWidget
  ]

signals =
  collect
  [ just $ makeSignal c_QDockWidget "allowedAreasChanged" c_ListenerDockWidgetAreas
  , test (qtVersion >= [4, 3]) $
    makeSignal c_QDockWidget "dockLocationChanged" c_ListenerDockWidgetArea
  , just $ makeSignal c_QDockWidget "featuresChanged" c_ListenerQDockWidgetFeatures
  , just $ makeSignal c_QDockWidget "topLevelChanged" c_ListenerBool
  , just $ makeSignal c_QDockWidget "visibilityChanged" c_ListenerBool
  ]

(e_DockWidgetFeature, bs_DockWidgetFeatures) =
  makeQtEnumBitspace (ident1 "QDockWidget" "DockWidgetFeature") "DockWidgetFeatures"
  [includeStd "QDockWidget"] $
  [ (0x0, ["no", "dock", "widget", "features"])
  , (0x1, ["dock", "widget", "closable"])
  , (0x2, ["dock", "widget", "movable"])
  , (0x4, ["dock", "widget", "floatable"])
  , (0x7, ["all", "dock", "widget", "features"])  -- Deprecated
  , (0x8, ["dock", "widget", "vertical", "title", "bar"])
  ]
