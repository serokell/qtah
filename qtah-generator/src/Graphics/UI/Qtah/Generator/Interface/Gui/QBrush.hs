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

module Graphics.UI.Qtah.Generator.Interface.Gui.QBrush (
  aModule,
  c_QBrush,
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
  mkCtor,
  mkMethod,
  mkMethod',
  mkProp,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (boolT, enumT, objT, refT, voidT)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_BrushStyle, e_GlobalColor)
import Graphics.UI.Qtah.Generator.Interface.Gui.QColor (c_QColor)
import Graphics.UI.Qtah.Generator.Interface.Gui.QImage (c_QImage)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPixmap (c_QPixmap)
import Graphics.UI.Qtah.Generator.Interface.Gui.QTransform (c_QTransform)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QBrush"]
  [ QtExport $ ExportClass c_QBrush ]

c_QBrush =
  addReqIncludes [includeStd "QBrush"] $
  classSetConversionToGc $
  classAddFeatures [Assignable, Copyable, Equatable] $
  classSetEntityPrefix "" $
  makeClass (ident "QBrush") Nothing [] $
  [ mkCtor "new" []
  , mkCtor "newWithColor" [objT c_QColor]
  , mkProp "color" $ objT c_QColor
    -- TODO mkConstMethod "gradient" [] $ ptrT $ constT $ objT c_QGradient
  , mkConstMethod "isOpaque" [] boolT
    -- TODO mkProp "matrix" $ objT c_QMatrix
  , mkMethod' "setColor" "setGlobalColor" [enumT e_GlobalColor] voidT
  , mkProp "style" $ enumT e_BrushStyle
  , mkMethod "swap" [refT $ objT c_QBrush] voidT
  , mkProp "texture" $ objT c_QPixmap
  , mkProp "textureImage" $ objT c_QImage
  , mkProp "transform" $ objT c_QTransform
  ]
