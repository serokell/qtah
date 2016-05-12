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

module Graphics.UI.Qtah.Internal.Interface.Core.QRectF (
  aModule,
  c_QRectF,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  Type (TBool, TObj, TVoid),
  addReqIncludes,
  classSetConversionToGc,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
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
import Graphics.UI.Qtah.Internal.Interface.Core.QMarginsF (c_QMarginsF)
import Graphics.UI.Qtah.Internal.Interface.Core.QPointF (c_QPointF)
import Graphics.UI.Qtah.Internal.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Internal.Interface.Core.QSizeF (c_QSizeF)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (qreal)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QRectF"]
  [ QtExport $ ExportClass c_QRectF ]

c_QRectF =
  addReqIncludes [includeStd "QRectF"] $
  classAddFeatures [Assignable, Copyable, Equatable] $
  classSetConversionToGc $
  makeClass (ident "QRectF") Nothing []
  (collect
   [ just $ mkCtor "newNull" []
   , test (qtVersion >= [4, 3]) $ mkCtor "newFromPoints" [TObj c_QPointF, TObj c_QPointF]
   , just $ mkCtor "newWithPointAndSize" [TObj c_QPointF, TObj c_QSizeF]
   , just $ mkCtor "newWithRect" [TObj c_QRect]
   , just $ mkCtor "newWithRaw" [qreal, qreal, qreal, qreal]
   ]) $
  collect
  [ just $ mkMethod "adjust" [qreal, qreal, qreal, qreal] TVoid
  , just $ mkConstMethod "adjusted" [qreal, qreal, qreal, qreal] $ TObj c_QRectF
  , just $ mkConstMethod "center" [] $ TObj c_QPointF
  , just $ mkConstMethod' "contains" "containsPoint" [TObj c_QPointF] TBool
  , just $ mkConstMethod' "contains" "containsRect" [TObj c_QRectF] TBool
  , test (qtVersion >= [4, 2]) $ mkConstMethod "intersected" [TObj c_QRectF] $ TObj c_QRectF
  , just $ mkConstMethod "intersects" [TObj c_QRectF] TBool
  , just $ mkConstMethod "isEmpty" [] TBool
  , just $ mkConstMethod "isNull" [] TBool
  , just $ mkConstMethod "isValid" [] TBool
  , test (qtVersion >= [5, 3]) $ mkConstMethod "marginsAdded" [TObj c_QMarginsF] $ TObj c_QRectF
  , test (qtVersion >= [5, 3]) $ mkConstMethod "marginsRemoved" [TObj c_QMarginsF] $ TObj c_QRectF
  , just $ mkMethod "moveBottom" [qreal] TVoid
  , just $ mkMethod "moveBottomLeft" [TObj c_QPointF] TVoid
  , just $ mkMethod "moveBottomRight" [TObj c_QPointF] TVoid
  , just $ mkMethod "moveCenter" [TObj c_QPointF] TVoid
  , just $ mkMethod "moveLeft" [qreal] TVoid
  , just $ mkMethod "moveRight" [qreal] TVoid
  , just $ mkMethod "moveTo" [TObj c_QPointF] TVoid
  , just $ mkMethod "moveTop" [qreal] TVoid
  , just $ mkMethod "moveTopLeft" [TObj c_QPointF] TVoid
  , just $ mkMethod "moveTopRight" [TObj c_QPointF] TVoid
  , just $ mkConstMethod "normalized" [] $ TObj c_QRectF
  , just $ mkMethod "setCoords" [qreal, qreal, qreal, qreal] TVoid
  , just $ mkMethod "setRect" [qreal, qreal, qreal, qreal] TVoid
  , just $ mkConstMethod "toAlignedRect" [] $ TObj c_QRect
  , just $ mkMethod "translate" [TObj c_QPointF] TVoid
  , just $ mkConstMethod "translated" [TObj c_QPointF] $ TObj c_QRectF
  , test (qtVersion >= [4, 2]) $ mkMethod "united" [TObj c_QRectF] $ TObj c_QRectF
  ] ++
  mkProps
  [ mkProp "bottom" qreal
  , mkProp "bottomLeft" $ TObj c_QPointF
  , mkProp "bottomRight" $ TObj c_QPointF
  , mkProp "height" qreal
  , mkProp "left" qreal
  , mkProp "right" qreal
  , mkProp "size" $ TObj c_QSizeF
  , mkProp "top" qreal
  , mkProp "topLeft" $ TObj c_QPointF
  , mkProp "topRight" $ TObj c_QPointF
  , mkProp "width" qreal
  , mkProp "x" qreal
  , mkProp "y" qreal
  ]
