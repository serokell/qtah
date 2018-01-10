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

module Graphics.UI.Qtah.Generator.Interface.Gui.QRegion (
  aModule,
  c_QRegion,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass, ExportEnum),
  addReqIncludes,
  classSetConversionToGc,
  classSetEntityPrefix,
  ident,
  ident1,
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
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_FillRule)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPolygon (c_QPolygon)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QRegion"]
  [ QtExport $ ExportClass c_QRegion
  , QtExport $ ExportEnum e_RegionType
  ]

c_QRegion =
  addReqIncludes [includeStd "QRegion"] $
  classSetConversionToGc $
  classAddFeatures [Assignable, Copyable, Equatable] $
  classSetEntityPrefix "" $
  makeClass (ident "QRegion") Nothing [] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newFromPoints" [intT, intT, intT, intT, enumT e_RegionType]
  , just $ mkCtor "newFromPolygon" [objT c_QPolygon]
  , just $ mkCtor "newFromPolygonAll" [objT c_QPolygon, enumT e_FillRule]
    -- TODO newFromBitmap (needs QBitmap)
  , just $ mkCtor "newFromRect" [objT c_QRect, enumT e_RegionType]
  , just $ mkConstMethod "boundingRect" [] $ objT c_QRect
  , just $ mkConstMethod' "contains" "containsPoint" [objT c_QPoint] boolT
  , just $ mkConstMethod' "contains" "containsRect" [objT c_QRect] boolT
  , test (qtVersion >= [4, 2]) $
    mkConstMethod' "intersected" "intersected" [objT c_QRegion] $ objT c_QRegion
  , test (qtVersion >= [4, 4]) $
    mkConstMethod' "intersected" "intersectedWithRect" [objT c_QRect] $ objT c_QRegion
  , test (qtVersion >= [4, 2]) $ mkConstMethod' "intersects" "intersects" [objT c_QRegion] boolT
  , test (qtVersion >= [4, 2]) $ mkConstMethod' "intersects" "intersectsRect" [objT c_QRect] boolT
  , just $ mkConstMethod "isEmpty" [] boolT
  , test (qtVersion >= [5, 0]) $ mkConstMethod "isNull" [] boolT
  , test (qtVersion >= [4, 6]) $ mkConstMethod "rectCount" [] intT
    -- TODO rects
    -- TODO setRects
  , test (qtVersion >= [4, 2]) $
    mkConstMethod "subtracted" [objT c_QRegion] $ objT c_QRegion
  , test (qtVersion >= [4, 8]) $ mkMethod "swap" [refT $ objT c_QRegion] voidT
  , just $ mkMethod' "translate" "translateByCoords" [intT, intT] voidT
  , just $ mkMethod' "translate" "translateByPoint" [objT c_QPoint] voidT
  , test (qtVersion >= [4, 1]) $
    mkConstMethod' "translated" "translatedByCoords" [intT, intT] $ objT c_QRegion
  , test (qtVersion >= [4, 1]) $
    mkConstMethod' "translated" "translatedByPoint" [objT c_QPoint] $ objT c_QRegion
  , test (qtVersion >= [4, 2]) $
    mkConstMethod' "united" "united" [objT c_QRegion] $ objT c_QRegion
  , test (qtVersion >= [4, 4]) $
    mkConstMethod' "united" "unitedWithRect" [objT c_QRect] $ objT c_QRegion
  , test (qtVersion >= [4, 2]) $
    mkConstMethod' "xored" "xored" [objT c_QRegion] $ objT c_QRegion
  ]

e_RegionType =
  makeQtEnum (ident1 "QRegion" "RegionType") [includeStd "QRegion"]
  [ (0, ["rectangle"])
  , (1, ["ellipse"])
  ]
