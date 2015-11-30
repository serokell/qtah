-- This file is part of Qtah.
--
-- Copyright 2015 Bryan Gardiner <bog@khumba.net>
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

module Graphics.UI.Qtah.Internal.Interface.Widgets.QRubberBand (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportEnum, ExportClass),
  Type (TEnum, TObj, TPtr, TVoid),
  addReqIncludes,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod,
  )
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Internal.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Internal.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QRubberBand"]
  [ QtExport $ ExportClass c_QRubberBand
  , QtExport $ ExportEnum e_Shape
  ]

c_QRubberBand =
  addReqIncludes [includeStd "QRubberBand"] $
  makeClass (ident "QRubberBand") Nothing [c_QWidget]
  [ mkCtor "new" [TEnum e_Shape]
  , mkCtor "newWithParent" [TEnum e_Shape, TPtr $ TObj c_QWidget]
  ]
  [ mkMethod "move" [TObj c_QPoint] TVoid
  , mkMethod "resize" [TObj c_QSize] TVoid
  , mkMethod "setGeometry" [TObj c_QRect] TVoid
  , mkConstMethod "shape" [] $ TEnum e_Shape
  ]

e_Shape =
  makeQtEnum (ident1 "QRubberBand" "Shape") [includeStd "QRubberBand"]
  [ (0, ["line"])
  , (1, ["rectangle"])
  ]
