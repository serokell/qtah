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

module Graphics.UI.Qtah.Internal.Interface.Widgets.QLayoutItem (
  aModule,
  c_QLayoutItem,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  Type (TBitspace, TBool, TInt, TObj, TPtr, TVoid),
  addReqIncludes,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkMethod,
  mkProp,
  mkProps,
  )
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Internal.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (bs_Alignment)
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.Widgets.QLayout (c_QLayout)
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QLayoutItem"]
  [ QtExport $ ExportClass c_QLayoutItem ]

c_QLayoutItem =
  addReqIncludes [includeStd "QLayoutItem"] $
  makeClass (ident "QLayoutItem") Nothing []
  [] $  -- Abstract.
  [ -- TODO controlTypes
    -- TODO expandingDirections
    mkConstMethod "hasHeightForWidth" [] TBool
  , mkConstMethod "heightForWidth" [TInt] TInt
  , mkMethod "invalidate" [] TVoid
  , mkConstMethod "isEmpty" [] TBool
  , mkMethod "layout" [] $ TPtr $ TObj c_QLayout
  , mkConstMethod "maximumSize" [] $ TObj c_QSize
  , mkConstMethod "minimumHeightForWidth" [TInt] TInt
  , mkConstMethod "minimumSize" [] $ TObj c_QSize
  , mkConstMethod "sizeHint" [] $ TObj c_QSize
    -- TODO spacerItem
  , mkConstMethod "widget" [] $ TPtr $ TObj c_QWidget
  ] ++
  mkProps
  [ mkProp "alignment" $ TBitspace bs_Alignment
  , mkProp "geometry" $ TObj c_QRect
  ]
