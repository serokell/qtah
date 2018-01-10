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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QRubberBand (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportEnum, ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod,
  )
import Foreign.Hoppy.Generator.Types (enumT, objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QRubberBand"]
  [ QtExport $ ExportClass c_QRubberBand
  , QtExport $ ExportEnum e_Shape
  ]

c_QRubberBand =
  addReqIncludes [includeStd "QRubberBand"] $
  classSetEntityPrefix "" $
  makeClass (ident "QRubberBand") Nothing [c_QWidget]
  [ mkCtor "new" [enumT e_Shape]
  , mkCtor "newWithParent" [enumT e_Shape, ptrT $ objT c_QWidget]
  , mkMethod "move" [objT c_QPoint] voidT
  , mkMethod "resize" [objT c_QSize] voidT
  , mkMethod "setGeometry" [objT c_QRect] voidT
  , mkConstMethod "shape" [] $ enumT e_Shape
  ]

e_Shape =
  makeQtEnum (ident1 "QRubberBand" "Shape") [includeStd "QRubberBand"]
  [ (0, ["line"])
  , (1, ["rectangle"])
  ]
