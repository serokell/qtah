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

module Graphics.UI.Qtah.Internal.Interface.Widgets.QAction (
  aModule,
  c_QAction,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportEnum, ExportClass),
  Type (TBool, TEnum, TObj, TPtr, TVoid),
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
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Flag (collect, just, test)
import Graphics.UI.Qtah.Internal.Flags (qtVersion)
import Graphics.UI.Qtah.Internal.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Listener (c_Listener, c_ListenerBool)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QActionGroup (c_QActionGroup)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QMenu (c_QMenu)
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

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
  makeClass (ident "QAction") Nothing
  [ c_QObject ]
  [ mkCtor "new" [TPtr $ TObj c_QObject]
  , mkCtor "newWithText" [TObj c_QString, TPtr $ TObj c_QObject]
    -- TODO newWithIconAndText
  ] $
  [ mkMethod "activate" [TEnum e_ActionEvent] TVoid
    -- TODO associatedGraphicsWidgets
    -- TODO associatedWidgets
  , mkMethod "hover" [] TVoid
  , mkConstMethod "parentWidget" [] $ TPtr $ TObj c_QWidget
  , mkConstMethod "priority" [] $ TEnum e_Priority
  , mkMethod "setDisabled" [TBool] TVoid
  , mkMethod "setPriority" [TEnum e_Priority] TVoid
    -- TODO setShortcuts
    -- TODO shortcuts
  , mkMethod "showStatusText" [TPtr $ TObj c_QWidget] TBool
  , mkMethod "toggle" [] TVoid
  , mkMethod "trigger" [] TVoid
  ] ++
  (mkProps . collect)
  [ just $ mkProp "actionGroup" $ TPtr $ TObj c_QActionGroup
  , just $ mkProp "autoRepeat" TBool
  , just $ mkBoolIsProp "checkable"
  , just $ mkBoolIsProp "checked"
    -- TODO data
  , just $ mkBoolIsProp "enabled"
    -- TODO font
    -- TODO icon
  , just $ mkProp "iconText" $ TObj c_QString
  , just $ mkBoolIsProp "iconVisibleInMenu"
  , just $ mkProp "menu" $ TPtr $ TObj c_QMenu
  , just $ mkProp "menuRole" $ TEnum e_MenuRole
  , just $ mkBoolIsProp "separator"
    -- TODO shortcut
    -- TODO shortcutContext
  , test (qtVersion < [5]) $ mkProp "softKeyRole" $ TEnum e_SoftKeyRole
  , just $ mkProp "statusTip" $ TObj c_QString
  , just $ mkProp "text" $ TObj c_QString
  , just $ mkProp "toolTip" $ TObj c_QString
  , just $ mkBoolIsProp "visible"
  , just $ mkProp "whatsThis" $ TObj c_QString
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
