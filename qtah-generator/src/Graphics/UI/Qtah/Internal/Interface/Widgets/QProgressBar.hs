-- This file is part of Qtah.
--
-- Copyright 2015 Bryan Gardiner <bog@khumba.net>
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

module Graphics.UI.Qtah.Internal.Interface.Widgets.QProgressBar (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportEnum, ExportClass),
  Type (TBitspace, TBool, TEnum, TInt, TObj, TPtr, TVoid),
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
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Internal.Flags (qtVersion)
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (bs_Alignment, e_Orientation)
import Graphics.UI.Qtah.Internal.Interface.Listener (c_ListenerInt)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QProgressBar"] $
  QtExport (ExportClass c_QProgressBar) :
  map QtExportSignal signals ++
  collect
  [ test (qtVersion >= [4, 1]) $ QtExport $ ExportEnum e_Direction
  ]

c_QProgressBar =
  addReqIncludes [includeStd "QProgressBar"] $
  makeClass (ident "QProgressBar") Nothing [c_QWidget]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  ] $
  collect
  [ just $ mkMethod "reset" [] TVoid
  , test (qtVersion >= [5, 0]) $ mkMethod "resetFormat" [] TVoid  -- ADDED-BETWEEN 4.8 5.4
  , just $ mkMethod "setRange" [TInt, TInt] TVoid
  , just $ mkConstMethod "text" [] $ TObj c_QString
  ] ++
  (mkProps . collect)
  [ just $ mkProp "alignment" $ TBitspace bs_Alignment
  , test (qtVersion >= [4, 2]) $ mkProp "format" $ TObj c_QString
  , test (qtVersion >= [4, 1]) $ mkProp "invertedAppearance" TBool
  , just $ mkProp "maximum" TInt
  , just $ mkProp "minimum" TInt
  , test (qtVersion >= [4, 1]) $ mkProp "orientation" $ TEnum e_Orientation
  , test (qtVersion >= [4, 1]) $ mkProp "textDirection" $ TEnum e_Direction
  , just $ mkBoolIsProp "textVisible"
  , just $ mkProp "value" TInt
  ]

signals =
  [ makeSignal c_QProgressBar "valueChanged" c_ListenerInt
  ]

-- Introduced in Qt 4.1.
e_Direction =
  makeQtEnum (ident1 "QProgressBar" "Direction") [includeStd "QProgressBar"]
  [ (0, ["top", "to", "bottom"])
  , (1, ["bottom", "to", "top"])
  ]
