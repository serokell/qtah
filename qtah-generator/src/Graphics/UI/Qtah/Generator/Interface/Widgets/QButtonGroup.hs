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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QButtonGroup (
  aModule,
  c_QButtonGroup,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkMethod',
  mkProp,
  )
import Foreign.Hoppy.Generator.Types (boolT, intT, objT, ptrT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QList (c_QListQAbstractButton)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  c_ListenerInt,
  c_ListenerIntBool,
  c_ListenerPtrQAbstractButton,
  c_ListenerPtrQAbstractButtonBool,
  )
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractButton (
  c_QAbstractButton,
  )
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QButtonGroup"] $
  (QtExport $ ExportClass c_QButtonGroup) :
  map QtExportSignal signals

c_QButtonGroup =
  addReqIncludes [includeStd "QButtonGroup"] $
  classSetEntityPrefix "" $
  makeClass (ident "QButtonGroup") Nothing [c_QObject] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QObject]
  , just $ mkMethod' "addButton" "addButton" [ptrT $ objT c_QAbstractButton] voidT
  , just $ mkMethod' "addButton" "addButtonWithId" [ptrT $ objT c_QAbstractButton, intT] voidT
  , test (qtVersion >= [4, 1]) $ mkConstMethod "button" [intT] $ ptrT $ objT c_QAbstractButton
  , just $ mkConstMethod "buttons" [] $ objT c_QListQAbstractButton
  , just $ mkConstMethod "checkedButton" [] $ ptrT $ objT c_QAbstractButton
  , test (qtVersion >= [4, 1]) $ mkConstMethod "checkedId" [] intT
  , just $ mkProp "exclusive" boolT
  , test (qtVersion >= [4, 1]) $ mkConstMethod "id" [ptrT $ objT c_QAbstractButton] intT
  , just $ mkMethod "removeButton" [ptrT $ objT c_QAbstractButton] voidT
  , test (qtVersion >= [4, 1]) $ mkMethod "setId" [ptrT $ objT c_QAbstractButton, intT] voidT
  ]

signals =
  collect
  [ just $ makeSignal c_QButtonGroup "buttonClicked" c_ListenerPtrQAbstractButton
  , just $ makeSignal c_QButtonGroup "buttonClickedId" c_ListenerInt
  , test (qtVersion >= [4, 2]) $
    makeSignal c_QButtonGroup "buttonPressed" c_ListenerPtrQAbstractButton
  , test (qtVersion >= [4, 2]) $ makeSignal c_QButtonGroup "buttonPressedId" c_ListenerInt
  , test (qtVersion >= [4, 2]) $
    makeSignal c_QButtonGroup "buttonReleased" c_ListenerPtrQAbstractButton
  , test (qtVersion >= [4, 2]) $ makeSignal c_QButtonGroup "buttonReleasedId" c_ListenerInt
  , test (qtVersion >= [5, 2]) $
    makeSignal c_QButtonGroup "buttonToggled" c_ListenerPtrQAbstractButtonBool
  , test (qtVersion >= [5, 2]) $
    makeSignal c_QButtonGroup "buttonToggledId" c_ListenerIntBool
  ]
