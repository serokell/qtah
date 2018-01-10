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

module Graphics.UI.Qtah.Generator.Interface.Gui.QPolygon (
  aModule,
  c_QPolygon,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  classSetConversionToGc,
  classSetEntityPrefix,
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
import Foreign.Hoppy.Generator.Types (boolT, enumT, intT, objT, refT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Core.QVector (c_QVectorQPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_FillRule)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QPolygon"]
  [ QtExport $ ExportClass c_QPolygon ]

c_QPolygon =
  addReqIncludes [includeStd "QPolygon"] $
  classSetConversionToGc $
  classAddFeatures [Assignable, Copyable, Equatable] $
  classSetEntityPrefix "" $
  makeClass (ident "QPolygon") Nothing [c_QVectorQPoint] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithSize" [intT]
  , just $ mkCtor "newWithPoints" [objT c_QVectorQPoint]
  , just $ mkCtor "newWithRectangle" [objT c_QRect, boolT]
  , just $ mkConstMethod "boundingRect" [] $ objT c_QRect
  , test (qtVersion >= [4, 3]) $ mkConstMethod "containsPoint"
    [objT c_QPoint, enumT e_FillRule] boolT
  , test (qtVersion >= [4, 3]) $ mkConstMethod "intersected" [objT c_QPolygon] $ objT c_QPolygon
  , just $ mkConstMethod "point" [intT] $ objT c_QPoint
  , just $ mkMethod "putPoints" [intT, intT, objT c_QPolygon, intT] voidT
  , just $ mkMethod "setPoint" [intT, objT c_QPoint] voidT
    -- OMIT setPoints
  , test (qtVersion >= [4, 3]) $ mkConstMethod "subtracted" [objT c_QPolygon] $ objT c_QPolygon
  , test (qtVersion >= [4, 8]) $ mkMethod "swap" [refT $ objT c_QPolygon] voidT
  , just $ mkMethod' "translate" "translateByRaw" [intT, intT] voidT
  , just $ mkMethod' "translate" "translateByPoint" [objT c_QPoint] voidT
  , test (qtVersion >= [4, 6]) $ mkConstMethod' "translated" "translatedByRaw"
    [intT, intT] $ objT c_QPolygon
  , test (qtVersion >= [4, 6]) $ mkConstMethod' "translated" "translatedByPoint"
    [objT c_QPoint] $ objT c_QPolygon
  , test (qtVersion >= [4, 3]) $ mkConstMethod "united" [objT c_QPolygon] $ objT c_QPolygon
  ]
