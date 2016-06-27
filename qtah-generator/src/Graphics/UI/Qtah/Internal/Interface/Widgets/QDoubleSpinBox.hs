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

module Graphics.UI.Qtah.Internal.Interface.Widgets.QDoubleSpinBox (
  aModule,
  c_QDoubleSpinBox,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  Type (TDouble, TInt, TObj, TPtr, TVoid),
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
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Listener (
  c_ListenerDouble,
  c_ListenerQString,
  )
import Graphics.UI.Qtah.Internal.Interface.Widgets.QAbstractSpinBox (c_QAbstractSpinBox)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QDoubleSpinBox"] $
  QtExport (ExportClass c_QDoubleSpinBox) :
  map QtExportSignal signals

c_QDoubleSpinBox =
  addReqIncludes [includeStd "QDoubleSpinBox"] $
  makeClass (ident "QDoubleSpinBox") Nothing [c_QAbstractSpinBox]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  ] $
  [ mkConstMethod "cleanText" [] $ TObj c_QString
  , mkMethod "setRange" [TDouble, TDouble] TVoid
  , mkConstMethod "textFromValue" [TDouble] $ TObj c_QString
  , mkConstMethod "valueFromText" [TObj c_QString] TDouble
  ] ++
  mkProps
  [ mkProp "decimals" TInt
  , mkProp "maximum" TDouble
  , mkProp "minimum" TDouble
  , mkProp "prefix" $ TObj c_QString
  , mkProp "singleStep" TDouble
  , mkProp "suffix" $ TObj c_QString
  , mkProp "value" TDouble
  ]

signals =
  [ makeSignal' c_QDoubleSpinBox "valueChanged" "valueChangedDouble" c_ListenerDouble
  , makeSignal' c_QDoubleSpinBox "valueChanged" "valueChangedString" c_ListenerQString
  ]
