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

module Graphics.UI.Qtah.Internal.Interface.Gui.QDoubleValidator (
  aModule,
  c_QDoubleValidator,
  e_Notation,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportEnum, ExportClass),
  Type (TDouble, TEnum, TInt, TObj, TPtr, TVoid),
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
import Graphics.UI.Qtah.Internal.Flag (collect, just, test)
import Graphics.UI.Qtah.Internal.Flags (qtVersion)
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Internal.Interface.Gui.QValidator (c_QValidator)

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
  , mkCtor "newWithParent" [TPtr $ TObj c_QObject]
  , mkCtor "newWithOptions" [TDouble, TDouble, TInt]
  , mkCtor "newWithOptionsAndParent" [TDouble, TDouble, TInt, TPtr $ TObj c_QObject]
  ] $
  [ mkMethod' "setRange" "setRange" [TDouble, TDouble] TVoid
  , mkMethod' "setRange" "setRangeAndDecimals" [TDouble, TDouble, TInt] TVoid
  ] ++
  (mkProps . collect)
  [ just $ mkProp "bottom" TDouble
  , just $ mkProp "decimals" TInt
  , test (qtVersion >= [4, 3]) $ mkProp "notation" $ TEnum e_Notation
  , just $ mkProp "top" TDouble
  ]

e_Notation =
  makeQtEnum (ident1 "QDoubleValidator" "Notation") [includeStd "QDoubleValidator"]
  [ (0, ["standard", "notation"])
  , (1, ["scientific", "notation"])
  ]
