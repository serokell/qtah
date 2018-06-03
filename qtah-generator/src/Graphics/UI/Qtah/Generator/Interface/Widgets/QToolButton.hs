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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QToolButton (
  aModule,
  c_QToolButton,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportEnum, ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkCtor,
  mkMethod,
  mkProp,
  )
import Foreign.Hoppy.Generator.Types (boolT, enumT, objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_ArrowType, e_ToolButtonStyle)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractButton (c_QAbstractButton)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAction (c_QAction)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QMenu (c_QMenu)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QToolButton"]
  [ QtExport $ ExportClass c_QToolButton
  , QtExport $ ExportEnum e_ToolButtonPopupMode
  ]

c_QToolButton =
  addReqIncludes [includeStd "QToolButton"] $
  classSetEntityPrefix "" $
  makeClass (ident "QToolButton") Nothing
  [ c_QAbstractButton ]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , mkProp "arrowType" $ enumT e_ArrowType
  , mkProp "autoRaise" boolT
  , mkProp "defaultAction" $ ptrT $ objT c_QAction
  , mkProp "menu" $ ptrT $ objT c_QMenu
  , mkProp "popupMode" $ enumT e_ToolButtonPopupMode
  , mkProp "toolButtonStyle" $ enumT e_ToolButtonStyle
  , mkMethod "showMenu" [] voidT
  ]

e_ToolButtonPopupMode =
  makeQtEnum (ident1 "QToolButton" "ToolButtonPopupMode") [includeStd "QToolButton"]
  [ (0, ["delayed", "popup"])
  , (1, ["menu", "button", "popup"])
  , (2, ["instant", "popup"])
  ]
