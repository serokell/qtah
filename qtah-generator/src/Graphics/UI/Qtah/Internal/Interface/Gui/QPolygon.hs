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

module Graphics.UI.Qtah.Internal.Interface.Gui.QPolygon (
  aModule,
  c_QPolygon,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  Type (TBool, TEnum, TInt, TObj, TRef, TToGc, TVoid),
  addReqIncludes,
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
import Graphics.UI.Qtah.Internal.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Internal.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Internal.Interface.Core.QVector (c_QVectorQPoint)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (e_FillRule)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QPolygon"]
  [ QtExport $ ExportClass c_QPolygon ]

c_QPolygon =
  addReqIncludes [includeStd "QPolygon"] $
  classAddFeatures [Assignable, Copyable, Equatable] $
  makeClass (ident "QPolygon") Nothing [c_QVectorQPoint]
  [ mkCtor "new" []
  , mkCtor "newWithSize" [TInt]
  , mkCtor "newWithPoints" [TObj c_QVectorQPoint]
  , mkCtor "newWithRectangle" [TObj c_QRect, TBool]
  ] $
  collect
  [ just $ mkConstMethod "boundingRect" [] $ TObj c_QRect
  , test (qtVersion >= [4, 3]) $ mkConstMethod "containsPoint"
    [TObj c_QPoint, TEnum e_FillRule] TBool
  , test (qtVersion >= [4, 3]) $ mkConstMethod "intersected"
    [TObj c_QPolygon] $ TToGc $ TObj c_QPolygon
  , just $ mkConstMethod "point" [TInt] $ TObj c_QPoint
  , just $ mkMethod "putPoints" [TInt, TInt, TObj c_QPolygon, TInt] TVoid
  , just $ mkMethod "setPoint" [TInt, TObj c_QPoint] TVoid
    -- OMIT setPoints
  , test (qtVersion >= [4, 3]) $ mkConstMethod "subtracted" [TObj c_QPolygon] $ TToGc $ TObj c_QPolygon
  , test (qtVersion >= [4, 8]) $ mkMethod "swap" [TRef $ TObj c_QPolygon] TVoid
  , just $ mkMethod' "translate" "translateByRaw" [TInt, TInt] TVoid
  , just $ mkMethod' "translate" "translateByPoint" [TObj c_QPoint] TVoid
  , test (qtVersion >= [4, 6]) $ mkConstMethod' "translated" "translatedByRaw"
    [TInt, TInt] $ TToGc $ TObj c_QPolygon
  , test (qtVersion >= [4, 6]) $ mkConstMethod' "translated" "translatedByPoint"
    [TObj c_QPoint] $ TToGc $ TObj c_QPolygon
  , test (qtVersion >= [4, 3]) $ mkConstMethod "united" [TObj c_QPolygon] $ TToGc $ TObj c_QPolygon
  ]
