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

module Graphics.UI.Qtah.Generator.Interface.Gui.QDoubleValidator (
  aModule,
  c_QDoubleValidator,
  e_Notation,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportEnum, ExportClass),
  addReqIncludes,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkCtor,
  mkMethod',
  mkProp,
  mkProps,
  )
import Foreign.Hoppy.Generator.Types (doubleT, enumT, intT, objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Flag (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Gui.QValidator (c_QValidator)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QDoubleValidator"] $
  (map QtExport . collect)
  [ just $ ExportClass c_QDoubleValidator
  , test (qtVersion >= [4, 3]) $ ExportEnum e_Notation
  ]

c_QDoubleValidator =
  addReqIncludes [includeStd "QDoubleValidator"] $
  makeClass (ident "QDoubleValidator") Nothing [c_QValidator]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [ptrT $ objT c_QObject]
  , mkCtor "newWithOptions" [doubleT, doubleT, intT]
  , mkCtor "newWithOptionsAndParent" [doubleT, doubleT, intT, ptrT $ objT c_QObject]
  ] $
  [ mkMethod' "setRange" "setRange" [doubleT, doubleT] voidT
  , mkMethod' "setRange" "setRangeAndDecimals" [doubleT, doubleT, intT] voidT
  ] ++
  (mkProps . collect)
  [ just $ mkProp "bottom" doubleT
  , just $ mkProp "decimals" intT
  , test (qtVersion >= [4, 3]) $ mkProp "notation" $ enumT e_Notation
  , just $ mkProp "top" doubleT
  ]

e_Notation =
  makeQtEnum (ident1 "QDoubleValidator" "Notation") [includeStd "QDoubleValidator"]
  [ (0, ["standard", "notation"])
  , (1, ["scientific", "notation"])
  ]
