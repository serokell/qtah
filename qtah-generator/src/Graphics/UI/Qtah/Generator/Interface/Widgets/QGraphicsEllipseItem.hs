-- This file is part of Qtah.
--
-- Copyright 2015-2017 Bryan Gardiner <bog@khumba.net>
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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QGraphicsEllipseItem (
  aModule,
  c_QGraphicsEllipseItem,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkCtor,
  )
import Foreign.Hoppy.Generator.Types (doubleT)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractGraphicsShapeItem
  (c_QAbstractGraphicsShapeItem)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QGraphicsEllipseItem"]
  [ QtExport $ ExportClass c_QGraphicsEllipseItem
  ]

c_QGraphicsEllipseItem =
  addReqIncludes [includeStd "QGraphicsEllipseItem"] $
  classSetEntityPrefix "" $
  makeClass (ident "QGraphicsEllipseItem") Nothing [c_QAbstractGraphicsShapeItem]
  [ mkCtor "new" []
  , mkCtor "newRectRaw" [doubleT, doubleT, doubleT, doubleT]
  ]