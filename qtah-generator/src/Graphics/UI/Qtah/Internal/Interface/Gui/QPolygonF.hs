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

module Graphics.UI.Qtah.Internal.Interface.Gui.QPolygonF (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  Type (TBool, TEnum, TInt, TObj, TRef, TVoid),
  addReqIncludes,
  classSetConversionToGc,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkMethod',
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Internal.Flags (qtVersion)
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QPointF (c_QPointF)
import Graphics.UI.Qtah.Internal.Interface.Core.QRectF (c_QRectF)
import Graphics.UI.Qtah.Internal.Interface.Core.QVector (c_QVectorQPointF)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (e_FillRule)
import Graphics.UI.Qtah.Internal.Interface.Gui.QPolygon (c_QPolygon)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QPolygonF"]
  [ QtExport $ ExportClass c_QPolygonF ]

c_QPolygonF =
  addReqIncludes [includeStd "QPolygonF"] $
  classAddFeatures [Assignable, Copyable, Equatable] $
  classSetConversionToGc $
  makeClass (ident "QPolygonF") Nothing [c_QVectorQPointF]
  [ mkCtor "new" []
  , mkCtor "newWithSize" [TInt]
  , mkCtor "newWithPoints" [TObj c_QVectorQPointF]
  , mkCtor "newWithPolygon" [TObj c_QPolygon]
  , mkCtor "newWithRectangle" [TObj c_QRectF]
  ] $
  collect
  [ just $ mkConstMethod "boundingRect" [] $ TObj c_QRectF
  , test (qtVersion >= [4, 3]) $ mkConstMethod "containsPoint"
    [TObj c_QPointF, TEnum e_FillRule] TBool
  , test (qtVersion >= [4, 3]) $ mkConstMethod "intersected" [TObj c_QPolygonF] $ TObj c_QPolygonF
  , just $ mkConstMethod "isClosed" [] TBool
  , test (qtVersion >= [4, 3]) $ mkConstMethod "subtracted" [TObj c_QPolygonF] $ TObj c_QPolygonF
  , test (qtVersion >= [4, 8]) $ mkMethod "swap" [TRef $ TObj c_QPolygonF] TVoid
  , just $ mkConstMethod "toPolygon" [] $ TObj c_QPolygon
  , just $ mkMethod' "translate" "translateByRaw" [TInt, TInt] TVoid
  , just $ mkMethod' "translate" "translateByPoint" [TObj c_QPointF] TVoid
  , test (qtVersion >= [4, 6]) $ mkConstMethod' "translated" "translatedByRaw"
    [TInt, TInt] $ TObj c_QPolygonF
  , test (qtVersion >= [4, 6]) $ mkConstMethod' "translated" "translatedByPoint"
    [TObj c_QPointF] $ TObj c_QPolygonF
  , test (qtVersion >= [4, 3]) $ mkConstMethod "united"
    [TObj c_QPolygonF] $ TObj c_QPolygonF
  ]
