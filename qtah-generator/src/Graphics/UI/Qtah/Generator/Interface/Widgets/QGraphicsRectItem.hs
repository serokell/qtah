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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QGraphicsRectItem (
  aModule,
  c_QGraphicsRectItem,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkMethod',
  mkConstMethod,
  mkCtor,
  )
import Foreign.Hoppy.Generator.Types (voidT, objT)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (qreal)
import Graphics.UI.Qtah.Generator.Interface.Core.QRectF (c_QRectF)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractGraphicsShapeItem
  (c_QAbstractGraphicsShapeItem)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QGraphicsRectItem"]
  [ QtExport $ ExportClass c_QGraphicsRectItem
  ]

c_QGraphicsRectItem =
  addReqIncludes [includeStd "QGraphicsRectItem"] $
  classSetEntityPrefix "" $
  makeClass (ident "QGraphicsRectItem") Nothing [c_QAbstractGraphicsShapeItem]
  [ mkCtor "new" []
  , mkCtor "newWithRaw" [qreal, qreal, qreal, qreal]
  , mkConstMethod "rect" [] $ objT c_QRectF
  , mkMethod' "setRect" "setRectRaw" [qreal, qreal, qreal, qreal] voidT
  ]
