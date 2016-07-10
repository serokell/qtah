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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QButtonGroup (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkMethod',
  mkProp,
  mkProps,
  )
import Foreign.Hoppy.Generator.Types (boolT, intT, objT, ptrT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QList (c_QListQAbstractButton)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Listener (
  c_ListenerInt,
  c_ListenerIntBool,
  c_ListenerPtrQAbstractButton,
  c_ListenerPtrQAbstractButtonBool,
  )
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractButton (c_QAbstractButton)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QButtonGroup"] $
  (QtExport $ ExportClass c_QButtonGroup) :
  map QtExportSignal signals

c_QButtonGroup =
  addReqIncludes [includeStd "QButtonGroup"] $
  makeClass (ident "QButtonGroup") Nothing [c_QObject]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [ptrT $ objT c_QObject]
  ] $
  collect
  [ just $ mkMethod' "addButton" "addButton" [ptrT $ objT c_QAbstractButton] voidT
  , just $ mkMethod' "addButton" "addButtonWithId" [ptrT $ objT c_QAbstractButton, intT] voidT
  , test (qtVersion >= [4, 1]) $ mkConstMethod "button" [intT] $ ptrT $ objT c_QAbstractButton
  , just $ mkConstMethod "buttons" [] $ objT c_QListQAbstractButton
  , just $ mkConstMethod "checkedButton" [] $ ptrT $ objT c_QAbstractButton
  , test (qtVersion >= [4, 1]) $ mkConstMethod "checkedId" [] intT
  , test (qtVersion >= [4, 1]) $ mkConstMethod "id" [ptrT $ objT c_QAbstractButton] intT
  , just $ mkMethod "removeButton" [ptrT $ objT c_QAbstractButton] voidT
  , test (qtVersion >= [4, 1]) $ mkMethod "setId" [ptrT $ objT c_QAbstractButton, intT] voidT
  ] ++
  mkProps
  [ mkProp "exclusive" boolT
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
