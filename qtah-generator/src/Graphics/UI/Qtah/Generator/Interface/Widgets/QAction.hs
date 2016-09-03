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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QAction (
  aModule,
  c_QAction,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportEnum, ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
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
import Foreign.Hoppy.Generator.Types (boolT, enumT, objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Flag (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Listener (c_Listener, c_ListenerBool)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QActionGroup (c_QActionGroup)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QMenu (c_QMenu)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QAction"] $
  QtExport (ExportClass c_QAction) :
  map QtExportSignal signals ++
  (map (QtExport . ExportEnum) . collect)
  [ just $ e_ActionEvent
  , just $ e_MenuRole
  , just $ e_Priority
  , test (qtVersion < [5]) $ e_SoftKeyRole
  ]

c_QAction =
  addReqIncludes [includeStd "QAction"] $
  classSetEntityPrefix "" $
  makeClass (ident "QAction") Nothing
  [ c_QObject ]
  [ mkCtor "new" [ptrT $ objT c_QObject]
  , mkCtor "newWithText" [objT c_QString, ptrT $ objT c_QObject]
    -- TODO newWithIconAndText
  ] $
  [ mkMethod "activate" [enumT e_ActionEvent] voidT
    -- TODO associatedGraphicsWidgets
    -- TODO associatedWidgets
  , mkMethod "hover" [] voidT
  , mkConstMethod "parentWidget" [] $ ptrT $ objT c_QWidget
  , mkConstMethod "priority" [] $ enumT e_Priority
  , mkMethod "setDisabled" [boolT] voidT
  , mkMethod "setPriority" [enumT e_Priority] voidT
    -- TODO setShortcuts
    -- TODO shortcuts
  , mkMethod "showStatusText" [ptrT $ objT c_QWidget] boolT
  , mkMethod "toggle" [] voidT
  , mkMethod "trigger" [] voidT
  ] ++
  (mkProps . collect)
  [ just $ mkProp "actionGroup" $ ptrT $ objT c_QActionGroup
  , just $ mkProp "autoRepeat" boolT
  , just $ mkBoolIsProp "checkable"
  , just $ mkBoolIsProp "checked"
    -- TODO data
  , just $ mkBoolIsProp "enabled"
    -- TODO font
    -- TODO icon
  , just $ mkProp "iconText" $ objT c_QString
  , just $ mkBoolIsProp "iconVisibleInMenu"
  , just $ mkProp "menu" $ ptrT $ objT c_QMenu
  , just $ mkProp "menuRole" $ enumT e_MenuRole
  , just $ mkBoolIsProp "separator"
    -- TODO shortcut
    -- TODO shortcutContext
  , test (qtVersion < [5]) $ mkProp "softKeyRole" $ enumT e_SoftKeyRole
  , just $ mkProp "statusTip" $ objT c_QString
  , just $ mkProp "text" $ objT c_QString
  , just $ mkProp "toolTip" $ objT c_QString
  , just $ mkBoolIsProp "visible"
  , just $ mkProp "whatsThis" $ objT c_QString
  ]

signals =
  [ makeSignal c_QAction "changed" c_Listener
  , makeSignal c_QAction "hovered" c_Listener
  , makeSignal c_QAction "toggled" c_ListenerBool
  , makeSignal c_QAction "triggered" c_ListenerBool
  ]

e_ActionEvent =
  makeQtEnum (ident1 "QAction" "ActionEvent") [includeStd "QAction"]
  [ (0, ["trigger"])
  , (1, ["hover"])
  ]

e_MenuRole =
  makeQtEnum (ident1 "QAction" "MenuRole") [includeStd "QAction"]
  [ (0, ["no", "role"])
  , (1, ["text", "heuristic", "role"])
  , (2, ["application", "specific", "role"])
  , (3, ["about", "qt", "role"])
  , (4, ["about", "role"])
  , (5, ["preferences", "role"])
  , (6, ["quit", "role"])
  ]

e_Priority =
  makeQtEnum (ident1 "QAction" "Priority") [includeStd "QAction"]
  [ (0, ["low", "priority"])
  , (128, ["normal", "priority"])
  , (256, ["high", "priority"])
  ]

-- | Removed in Qt 5.
e_SoftKeyRole =
  makeQtEnum (ident1 "QAction" "SoftKeyRole") [includeStd "QAction"]
  [ (0, ["no", "soft", "key"])
  , (1, ["positive", "soft", "key"])
  , (2, ["negative", "soft", "key"])
  , (3, ["select", "soft", "key"])
  ]
