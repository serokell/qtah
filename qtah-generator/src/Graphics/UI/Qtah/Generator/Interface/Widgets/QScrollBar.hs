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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QScrollBar (
  aModule,
  c_QScrollBar
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
import Foreign.Hoppy.Generator.Types (enumT, objT, ptrT)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_Orientation)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractSlider (c_QAbstractSlider)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QScrollBar"]
  [ QtExport $ ExportClass c_QScrollBar ]

c_QScrollBar =
  addReqIncludes [includeStd "QScrollBar"] $
  classSetEntityPrefix "" $
  makeClass (ident "QScrollBar") Nothing [c_QAbstractSlider]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , mkCtor "newWithOrientation" [enumT e_Orientation]
  , mkCtor "newWithOrientationAndParent" [enumT e_Orientation, ptrT $ objT c_QWidget]
  ]
