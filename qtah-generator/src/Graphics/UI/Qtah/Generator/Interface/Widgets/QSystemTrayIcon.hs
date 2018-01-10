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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QSystemTrayIcon (
  aModule,
  c_QSystemTrayIcon,
  e_ActivationReason,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass, ExportEnum),
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
  mkMethod',
  mkProp,
  mkStaticMethod,
  )
import Foreign.Hoppy.Generator.Types (boolT, enumT, intT, objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Gui.QIcon (c_QIcon)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  c_Listener,
  c_ListenerQSystemTrayIconActivationReason,
  )
import Graphics.UI.Qtah.Generator.Interface.Widgets.QMenu (c_QMenu)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QSystemTrayIcon"] $
  QtExport (ExportClass c_QSystemTrayIcon) :
  map QtExportSignal signals ++
  [ QtExport $ ExportEnum e_ActivationReason
  , QtExport $ ExportEnum e_MessageIcon
  ]

c_QSystemTrayIcon =
  addReqIncludes [includeStd "QSystemTrayIcon"] $
  classSetEntityPrefix "" $
  makeClass (ident "QSystemTrayIcon") Nothing [c_QObject]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [ptrT $ objT c_QObject]
  , mkCtor "newWithIcon" [objT c_QIcon]
  , mkCtor "newWithIconAndParent" [objT c_QIcon, ptrT $ objT c_QObject]
  , mkProp "contextMenu" $ ptrT $ objT c_QMenu
  , mkConstMethod "geometry" [] $ objT c_QRect
  , mkMethod "hide" [] voidT
  , mkProp "icon" $ objT c_QIcon
  , mkStaticMethod "isSystemTrayAvailable" [] boolT
  , mkMethod "show" [] voidT
  , mkMethod' "showMessage" "showMessage" [objT c_QString, objT c_QString] voidT
  , mkMethod' "showMessage" "showMessageAll"
    [objT c_QString, objT c_QString, enumT e_MessageIcon, intT] voidT
  , mkStaticMethod "supportsMessages" [] boolT
  , mkProp "toolTip" $ objT c_QString
  , mkBoolIsProp "visible"
  ]

signals =
  [ makeSignal c_QSystemTrayIcon "activated" c_ListenerQSystemTrayIconActivationReason
  , makeSignal c_QSystemTrayIcon "messageClicked" c_Listener
  ]

e_ActivationReason =
  makeQtEnum (ident1 "QSystemTrayIcon" "ActivationReason") [includeStd "QSystemTrayIcon"]
  [ (0, ["unknown"])
  , (1, ["context"])
  , (2, ["double", "click"])
  , (3, ["trigger"])
  , (4, ["middle", "click"])
  ]

e_MessageIcon =
  makeQtEnum (ident1 "QSystemTrayIcon" "MessageIcon") [includeStd "QSystemTrayIcon"]
  [ (0, ["no", "icon"])
  , (1, ["information"])
  , (2, ["warning"])
  , (3, ["critical"])
  ]
