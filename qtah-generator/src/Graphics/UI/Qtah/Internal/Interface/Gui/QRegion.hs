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

module Graphics.UI.Qtah.Internal.Interface.Gui.QRegion (
  aModule,
  c_QRegion,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass, ExportEnum),
  Type (TBool, TEnum, TInt, TObj, TRef, TToGc, TVoid),
  addReqIncludes,
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
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Internal.Flags (qtVersion)
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Internal.Interface.Core.QRect (c_QRect)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QRegion"]
  [ QtExport $ ExportClass c_QRegion
  , QtExport $ ExportEnum e_RegionType
  ]

c_QRegion =
  addReqIncludes [includeStd "QRegion"] $
  classAddFeatures [Assignable, Copyable, Equatable] $
  makeClass (ident "QRegion") Nothing []
  [ mkCtor "new" []
  , mkCtor "newFromPoints" [TInt, TInt, TInt, TInt, TEnum e_RegionType]
    -- TODO newFromPolygon (needs QPolygon)
    -- TODO newFromBitmap (needs QBitmap)
  , mkCtor "newFromRect" [TObj c_QRect, TEnum e_RegionType]
  ] $
  collect
  [ just $ mkConstMethod "boundingRect" [] $ TObj c_QRect
  , just $ mkConstMethod' "contains" "containsPoint" [TObj c_QPoint] TBool
  , just $ mkConstMethod' "contains" "containsRect" [TObj c_QRect] TBool
  , test (qtVersion >= [4, 2]) $
    mkConstMethod' "intersected" "intersected" [TObj c_QRegion] $ TToGc $ TObj c_QRegion
  , test (qtVersion >= [4, 4]) $
    mkConstMethod' "intersected" "intersectedWithRect" [TObj c_QRect] $ TToGc $ TObj c_QRegion
  , test (qtVersion >= [4, 2]) $ mkConstMethod' "intersects" "intersects" [TObj c_QRegion] TBool
  , test (qtVersion >= [4, 2]) $ mkConstMethod' "intersects" "intersectsRect" [TObj c_QRect] TBool
  , just $ mkConstMethod "isEmpty" [] TBool
  , test (qtVersion >= [5, 0]) $ mkConstMethod "isNull" [] TBool
  , test (qtVersion >= [4, 6]) $ mkConstMethod "rectCount" [] TInt
    -- TODO rects
    -- TODO setRects
  , test (qtVersion >= [4, 2]) $
    mkConstMethod "subtracted" [TObj c_QRegion] $ TToGc $ TObj c_QRegion
  , test (qtVersion >= [4, 8]) $ mkMethod "swap" [TRef $ TObj c_QRegion] TVoid
  , just $ mkMethod' "translate" "translateByCoords" [TInt, TInt] TVoid
  , just $ mkMethod' "translate" "translateByPoint" [TObj c_QPoint] TVoid
  , test (qtVersion >= [4, 1]) $
    mkConstMethod' "translated" "translatedByCoords" [TInt, TInt] $ TToGc $ TObj c_QRegion
  , test (qtVersion >= [4, 1]) $
    mkConstMethod' "translated" "translatedByPoint" [TObj c_QPoint] $ TToGc $ TObj c_QRegion
  , test (qtVersion >= [4, 2]) $
    mkConstMethod' "united" "united" [TObj c_QRegion] $ TToGc $ TObj c_QRegion
  , test (qtVersion >= [4, 4]) $
    mkConstMethod' "united" "unitedWithRect" [TObj c_QRect] $ TToGc $ TObj c_QRegion
  , test (qtVersion >= [4, 2]) $
    mkConstMethod' "xored" "xored" [TObj c_QRegion] $ TToGc $ TObj c_QRegion
  ]

e_RegionType =
  makeQtEnum (ident1 "QRegion" "RegionType") [includeStd "QRegion"]
  [ (0, ["rectangle"])
  , (1, ["ellipse"])
  ]
