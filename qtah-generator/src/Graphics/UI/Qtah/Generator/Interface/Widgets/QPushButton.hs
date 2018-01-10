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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QPushButton (
  aModule,
  c_QPushButton,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkBoolIsProp,
  mkCtor,
  mkMethod,
  mkProp,
  )
import Foreign.Hoppy.Generator.Types (boolT, objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Gui.QIcon (c_QIcon)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractButton (c_QAbstractButton)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QMenu (c_QMenu)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QPushButton"]
  [ QtExport $ ExportClass c_QPushButton ]

c_QPushButton =
  addReqIncludes [includeStd "QPushButton"] $
  classSetEntityPrefix "" $
  makeClass (ident "QPushButton") Nothing
  [ c_QAbstractButton ]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , mkCtor "newWithText" [objT c_QString]
  , mkCtor "newWithTextAndParent" [objT c_QString, ptrT $ objT c_QWidget]
  , mkCtor "newWithIconAndText" [objT c_QIcon, objT c_QString]
  , mkCtor "newWithIconAndTextAndParent" [objT c_QIcon, objT c_QString, ptrT $ objT c_QWidget]
  , mkProp "autoDefault" boolT
  , mkBoolIsProp "default"
  , mkBoolIsProp "flat"
  , mkProp "menu" $ ptrT $ objT c_QMenu
  , mkMethod "showMenu" [] voidT
  ]
