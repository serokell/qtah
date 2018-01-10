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

module Graphics.UI.Qtah.Generator.Interface.Core.QMarginsF (
  aModule,
  c_QMarginsF,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  Operator (OpAddAssign, OpDivideAssign, OpMultiplyAssign, OpSubtractAssign),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod',
  mkProp,
  operatorPreferredExtName',
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (boolT, objT, refT)
import Graphics.UI.Qtah.Generator.Interface.Core.QMargins (c_QMargins)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (qreal)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

minVersion = [5, 3]

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QMarginsF"] minVersion
  [ QtExport $ ExportClass c_QMarginsF ]

c_QMarginsF =
  addReqIncludes [includeStd "QMarginsF"] $
  classAddFeatures [Assignable, Copyable, Equatable] $
  classSetEntityPrefix "" $
  makeClass (ident "QMarginsF") Nothing []
  [ mkCtor "newNull" []
  , mkCtor "new" [qreal, qreal, qreal, qreal]
  , mkCtor "newWithMargins" [objT c_QMargins]
  , mkProp "bottom" qreal
  , mkConstMethod "isNull" [] boolT
  , mkProp "left" qreal
  , mkProp "right" qreal
  , mkProp "top" qreal
  , mkMethod' OpAddAssign (operatorPreferredExtName' OpAddAssign)
    [objT c_QMarginsF] $ refT $ objT c_QMarginsF
  , mkMethod' OpAddAssign (operatorPreferredExtName' OpAddAssign ++ "Real")
    [qreal] $ refT $ objT c_QMarginsF
  , mkMethod' OpSubtractAssign (operatorPreferredExtName' OpSubtractAssign)
    [objT c_QMarginsF] $ refT $ objT c_QMarginsF
  , mkMethod' OpSubtractAssign (operatorPreferredExtName' OpSubtractAssign ++ "Real")
    [qreal] $ refT $ objT c_QMarginsF
  , mkMethod' OpMultiplyAssign (operatorPreferredExtName' OpMultiplyAssign)
    [qreal] $ refT $ objT c_QMarginsF
  , mkMethod' OpDivideAssign (operatorPreferredExtName' OpDivideAssign)
    [qreal] $ refT $ objT c_QMarginsF
  ]
