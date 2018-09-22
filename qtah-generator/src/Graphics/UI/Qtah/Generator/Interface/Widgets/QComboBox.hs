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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QComboBox (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkCtor,
  mkMethod,
  mkMethod',
  mkProp
  )
import Foreign.Hoppy.Generator.Types (intT, objT, ptrT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QVariant (c_QVariant)
import Graphics.UI.Qtah.Generator.Interface.Gui.QIcon (c_QIcon)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  c_ListenerInt,
  c_ListenerQString,
  )
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QComboBox"] $
  QtExport (ExportClass c_QComboBox) :
  map QtExportSignal signals

c_QComboBox =
  addReqIncludes [includeStd "QComboBox"] $
  classSetEntityPrefix "" $
  makeClass (ident "QComboBox") Nothing [c_QWidget] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , just $ mkMethod "addItem" [objT c_QString] voidT
  , just $ mkMethod' "addItem" "addItemWithData" [objT c_QString, objT c_QVariant] voidT
  , just $ mkMethod' "addItem" "addItemWithIcon" [objT c_QIcon, objT c_QString] voidT
  , just $ mkMethod' "addItem" "addItemWithIconAndData"
    [objT c_QIcon, objT c_QString, objT c_QVariant] voidT
  , just $ mkProp "currentIndex" intT
  , just $ mkProp "currentText" $ objT c_QString
  ]

signals =
  [ makeSignal c_QComboBox "activated" c_ListenerInt
  , makeSignal' c_QComboBox "activated" "activatedString" c_ListenerQString
  , makeSignal c_QComboBox "currentIndexChanged" c_ListenerInt
  , makeSignal' c_QComboBox "currentIndexChanged" "currentIndexChangedString" c_ListenerQString
  ]
