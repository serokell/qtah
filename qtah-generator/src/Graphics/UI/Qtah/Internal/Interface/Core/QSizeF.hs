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

module Graphics.UI.Qtah.Internal.Interface.Core.QSizeF (
  aModule,
  c_QSizeF,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  Operator (OpAddAssign, OpDivideAssign, OpMultiplyAssign, OpSubtractAssign),
  Type (TBool, TEnum, TObj, TRef, TVoid),
  addReqIncludes,
  classSetConversionToGc,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkProp,
  mkProps,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Internal.Flags (qtVersion)
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (e_AspectRatioMode, qreal)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QSizeF"]
  [ QtExport $ ExportClass c_QSizeF ]

c_QSizeF =
  addReqIncludes [includeStd "QSizeF"] $
  classAddFeatures [Assignable, Copyable, Equatable] $
  classSetConversionToGc $
  makeClass (ident "QSizeF") Nothing []
  [ mkCtor "newNull" []
  , mkCtor "new" [qreal, qreal]
  , mkCtor "newWithSize" [TObj c_QSize]
  ] $
  collect
  [ just $ mkConstMethod "boundedTo" [TObj c_QSizeF] $ TObj c_QSizeF
  , just $ mkConstMethod "expandedTo" [TObj c_QSizeF] $ TObj c_QSizeF
  , just $ mkConstMethod "isEmpty" [] TBool
  , just $ mkConstMethod "isNull" [] TBool
  , just $ mkConstMethod "isValid" [] TBool
  , just $ mkMethod "scale" [TObj c_QSizeF, TEnum e_AspectRatioMode] TVoid
  , test (qtVersion >= [5, 0]) $
    mkConstMethod "scaled" [TObj c_QSizeF, TEnum e_AspectRatioMode] $ TObj c_QSizeF
  , just $ mkConstMethod "toSize" [] $ TObj c_QSize
  , just $ mkMethod "transpose" [] TVoid
  , test (qtVersion >= [5, 0]) $ mkConstMethod "transposed" [] $ TObj c_QSizeF
  , just $ mkMethod OpAddAssign [TObj c_QSizeF] $ TRef $ TObj c_QSizeF
  , just $ mkMethod OpSubtractAssign [TObj c_QSizeF] $ TObj c_QSizeF
  , just $ mkMethod OpMultiplyAssign [qreal] $ TRef $ TObj c_QSizeF
  , just $ mkMethod OpDivideAssign [qreal] $ TRef $ TObj c_QSizeF
  ] ++
  mkProps
  [ mkProp "height" qreal
  , mkProp "width" qreal
  ]
