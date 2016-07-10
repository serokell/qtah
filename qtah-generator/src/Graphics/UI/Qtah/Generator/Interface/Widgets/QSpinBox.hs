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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QSpinBox (
  aModule,
  c_QSpinBox,
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
  mkProp,
  mkProps,
  )
import Foreign.Hoppy.Generator.Types (intT, objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Flag (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Listener (
  c_ListenerInt,
  c_ListenerQString,
  )
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractSpinBox (c_QAbstractSpinBox)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QSpinBox"] $
  QtExport (ExportClass c_QSpinBox) :
  map QtExportSignal signals

c_QSpinBox =
  addReqIncludes [includeStd "QSpinBox"] $
  makeClass (ident "QSpinBox") Nothing [c_QAbstractSpinBox]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  ] $
  [ mkConstMethod "cleanText" [] $ objT c_QString
  , mkMethod "setRange" [intT, intT] voidT
  ] ++
  (mkProps . collect)
  [ test (qtVersion >= [5, 2]) $ mkProp "displayIntegerBase" intT
  , just $ mkProp "maximum" intT
  , just $ mkProp "minimum" intT
  , just $ mkProp "prefix" $ objT c_QString
  , just $ mkProp "singleStep" intT
  , just $ mkProp "suffix" $ objT c_QString
  , just $ mkProp "value" intT
  ]

signals =
  [ makeSignal' c_QSpinBox "valueChanged" "valueChangedInt" c_ListenerInt
  , makeSignal' c_QSpinBox "valueChanged" "valueChangedString" c_ListenerQString
  ]
