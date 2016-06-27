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

module Graphics.UI.Qtah.Internal.Interface.Widgets.QSpinBox (
  aModule,
  c_QSpinBox,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  Type (TInt, TObj, TPtr, TVoid),
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
import Graphics.UI.Qtah.Internal.Flag (collect, just, test)
import Graphics.UI.Qtah.Internal.Flags (qtVersion)
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Listener (
  c_ListenerInt,
  c_ListenerQString,
  )
import Graphics.UI.Qtah.Internal.Interface.Widgets.QAbstractSpinBox (c_QAbstractSpinBox)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

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
  , mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  ] $
  [ mkConstMethod "cleanText" [] $ TObj c_QString
  , mkMethod "setRange" [TInt, TInt] TVoid
  ] ++
  (mkProps . collect)
  [ test (qtVersion >= [5, 2]) $ mkProp "displayIntegerBase" TInt
  , just $ mkProp "maximum" TInt
  , just $ mkProp "minimum" TInt
  , just $ mkProp "prefix" $ TObj c_QString
  , just $ mkProp "singleStep" TInt
  , just $ mkProp "suffix" $ TObj c_QString
  , just $ mkProp "value" TInt
  ]

signals =
  [ makeSignal' c_QSpinBox "valueChanged" "valueChangedInt" c_ListenerInt
  , makeSignal' c_QSpinBox "valueChanged" "valueChangedString" c_ListenerQString
  ]
