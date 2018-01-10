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

module Graphics.UI.Qtah.Generator.Interface.Core.QSizeF (
  aModule,
  c_QSizeF,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  Operator (OpAddAssign, OpDivideAssign, OpMultiplyAssign, OpSubtractAssign),
  addReqIncludes,
  classSetConversionToGc,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkProp,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (boolT, enumT, objT, refT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_AspectRatioMode, qreal)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QSizeF"]
  [ QtExport $ ExportClass c_QSizeF ]

c_QSizeF =
  addReqIncludes [includeStd "QSizeF"] $
  classSetConversionToGc $
  classAddFeatures [Assignable, Copyable, Equatable] $
  classSetEntityPrefix "" $
  makeClass (ident "QSizeF") Nothing [] $
  collect
  [ just $ mkCtor "newNull" []
  , just $ mkCtor "new" [qreal, qreal]
  , just $ mkCtor "newWithSize" [objT c_QSize]
  , just $ mkConstMethod "boundedTo" [objT c_QSizeF] $ objT c_QSizeF
  , just $ mkConstMethod "expandedTo" [objT c_QSizeF] $ objT c_QSizeF
  , just $ mkProp "height" qreal
  , just $ mkConstMethod "isEmpty" [] boolT
  , just $ mkConstMethod "isNull" [] boolT
  , just $ mkConstMethod "isValid" [] boolT
  , just $ mkMethod "scale" [objT c_QSizeF, enumT e_AspectRatioMode] voidT
  , test (qtVersion >= [5, 0]) $
    mkConstMethod "scaled" [objT c_QSizeF, enumT e_AspectRatioMode] $ objT c_QSizeF
  , just $ mkConstMethod "toSize" [] $ objT c_QSize
  , just $ mkMethod "transpose" [] voidT
  , test (qtVersion >= [5, 0]) $ mkConstMethod "transposed" [] $ objT c_QSizeF
  , just $ mkProp "width" qreal
  , just $ mkMethod OpAddAssign [objT c_QSizeF] $ refT $ objT c_QSizeF
  , just $ mkMethod OpSubtractAssign [objT c_QSizeF] $ objT c_QSizeF
  , just $ mkMethod OpMultiplyAssign [qreal] $ refT $ objT c_QSizeF
  , just $ mkMethod OpDivideAssign [qreal] $ refT $ objT c_QSizeF
  ]
