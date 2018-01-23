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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QLayoutItem (
  aModule,
  c_QLayoutItem,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkMethod,
  mkProp,
  )
import Foreign.Hoppy.Generator.Types (bitspaceT, boolT, intT, objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (bs_Alignment, bs_Orientations)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QLayout (c_QLayout)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QLayoutItem"]
  [ QtExport $ ExportClass c_QLayoutItem ]

c_QLayoutItem =
  addReqIncludes [includeStd "QLayoutItem"] $
  classSetEntityPrefix "" $
  makeClass (ident "QLayoutItem") Nothing []
  -- Abstract.
  [ mkProp "alignment" $ bitspaceT bs_Alignment
    -- TODO controlTypes
  , mkConstMethod "expandingDirections" [] $ bitspaceT bs_Orientations
  , mkProp "geometry" $ objT c_QRect
  , mkConstMethod "hasHeightForWidth" [] boolT
  , mkConstMethod "heightForWidth" [intT] intT
  , mkMethod "invalidate" [] voidT
  , mkConstMethod "isEmpty" [] boolT
  , mkMethod "layout" [] $ ptrT $ objT c_QLayout
  , mkConstMethod "maximumSize" [] $ objT c_QSize
  , mkConstMethod "minimumHeightForWidth" [intT] intT
  , mkConstMethod "minimumSize" [] $ objT c_QSize
  , mkConstMethod "sizeHint" [] $ objT c_QSize
    -- TODO spacerItem
  , mkMethod "widget" [] $ ptrT $ objT c_QWidget
  ]
