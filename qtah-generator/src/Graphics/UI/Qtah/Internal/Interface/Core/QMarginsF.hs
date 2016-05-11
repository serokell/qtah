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

module Graphics.UI.Qtah.Internal.Interface.Core.QMarginsF (
  aModule,
  c_QMarginsF,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  Operator (OpAddAssign, OpDivideAssign, OpMultiplyAssign, OpSubtractAssign),
  Type (TBool, TObj, TRef),
  addReqIncludes,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod',
  mkProp,
  mkProps,
  operatorPreferredExtName',
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Equatable),
  classAddFeatures,
  )
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QMargins (c_QMargins)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (qreal)

{-# ANN module "HLint: ignore Use camelCase" #-}

minVersion = [5, 3]

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QMarginsF"] minVersion
  [ QtExport $ ExportClass c_QMarginsF ]

c_QMarginsF =
  addReqIncludes [includeStd "QMarginsF"] $
  classAddFeatures [Assignable, Copyable, Equatable] $
  makeClass (ident "QMarginsF") Nothing []
  [ mkCtor "newNull" []
  , mkCtor "new" [qreal, qreal, qreal, qreal]
  , mkCtor "newWithMargins" [TObj c_QMargins]
  ] $
  [ mkConstMethod "isNull" [] TBool
  , mkMethod' OpAddAssign (operatorPreferredExtName' OpAddAssign)
    [TObj c_QMarginsF] $ TRef $ TObj c_QMarginsF
  , mkMethod' OpAddAssign (operatorPreferredExtName' OpAddAssign ++ "Real")
    [qreal] $ TRef $ TObj c_QMarginsF
  , mkMethod' OpSubtractAssign (operatorPreferredExtName' OpSubtractAssign)
    [TObj c_QMarginsF] $ TRef $ TObj c_QMarginsF
  , mkMethod' OpSubtractAssign (operatorPreferredExtName' OpSubtractAssign ++ "Real")
    [qreal] $ TRef $ TObj c_QMarginsF
  , mkMethod' OpMultiplyAssign (operatorPreferredExtName' OpMultiplyAssign)
    [qreal] $ TRef $ TObj c_QMarginsF
  , mkMethod' OpDivideAssign (operatorPreferredExtName' OpDivideAssign)
    [qreal] $ TRef $ TObj c_QMarginsF
  ] ++
  mkProps
  [ mkProp "bottom" qreal
  , mkProp "left" qreal
  , mkProp "right" qreal
  , mkProp "top" qreal
  ]
