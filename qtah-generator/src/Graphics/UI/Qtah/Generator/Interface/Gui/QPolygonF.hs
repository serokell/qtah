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

module Graphics.UI.Qtah.Generator.Interface.Gui.QPolygonF (
  aModule,
  c_QPolygonF
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
import Graphics.UI.Qtah.Generator.Interface.Core.QPointF (c_QPointF)
import Graphics.UI.Qtah.Generator.Interface.Core.QRectF (c_QRectF)
import Graphics.UI.Qtah.Generator.Interface.Core.QVector (c_QVectorQPointF)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_FillRule)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPolygon (c_QPolygon)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QPolygonF"]
  [ QtExport $ ExportClass c_QPolygonF ]

c_QPolygonF =
  addReqIncludes [includeStd "QPolygonF"] $
  classSetConversionToGc $
  classAddFeatures [Assignable, Copyable, Equatable] $
  classSetEntityPrefix "" $
  makeClass (ident "QPolygonF") Nothing [c_QVectorQPointF] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithSize" [intT]
  , just $ mkCtor "newWithPoints" [objT c_QVectorQPointF]
  , just $ mkCtor "newWithPolygon" [objT c_QPolygon]
  , just $ mkCtor "newWithRectangle" [objT c_QRectF]
  , just $ mkConstMethod "boundingRect" [] $ objT c_QRectF
  , test (qtVersion >= [4, 3]) $ mkConstMethod "containsPoint"
    [objT c_QPointF, enumT e_FillRule] boolT
  , test (qtVersion >= [4, 3]) $ mkConstMethod "intersected" [objT c_QPolygonF] $ objT c_QPolygonF
  , just $ mkConstMethod "isClosed" [] boolT
  , test (qtVersion >= [4, 3]) $ mkConstMethod "subtracted" [objT c_QPolygonF] $ objT c_QPolygonF
  , test (qtVersion >= [4, 8]) $ mkMethod "swap" [refT $ objT c_QPolygonF] voidT
  , just $ mkConstMethod "toPolygon" [] $ objT c_QPolygon
  , just $ mkMethod' "translate" "translateByRaw" [intT, intT] voidT
  , just $ mkMethod' "translate" "translateByPoint" [objT c_QPointF] voidT
  , test (qtVersion >= [4, 6]) $ mkConstMethod' "translated" "translatedByRaw"
    [intT, intT] $ objT c_QPolygonF
  , test (qtVersion >= [4, 6]) $ mkConstMethod' "translated" "translatedByPoint"
    [objT c_QPointF] $ objT c_QPolygonF
  , test (qtVersion >= [4, 3]) $ mkConstMethod "united"
    [objT c_QPolygonF] $ objT c_QPolygonF
  ]
