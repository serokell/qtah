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

module Graphics.UI.Qtah.Internal.Interface.Widgets.QAbstractScrollArea (
  aModule,
  c_QAbstractScrollArea,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  Type (TBitspace, TEnum, TObj, TPtr, TVoid),
  addReqIncludes,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkProp,
  mkProps,
  )
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (bs_Alignment, e_ScrollBarPolicy)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QAbstractScrollArea"]
  [ QtExport $ ExportClass c_QAbstractScrollArea ]

c_QAbstractScrollArea =
  addReqIncludes [includeStd "QAbstractScrollArea"] $
  makeClass (ident "QAbstractScrollArea") Nothing [c_QWidget]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  ] $
  [ mkMethod "addScrollBarWidget" [TPtr $ TObj c_QWidget, TBitspace bs_Alignment] TVoid
  , mkConstMethod "maximumViewportSize" [] $ TObj c_QSize
    -- TODO scrollBarWidgets
  ] ++
  mkProps
  [ mkProp "cornerWidget" $ TPtr $ TObj c_QWidget
    -- TODO horizontalScrollBar
  , mkProp "horizontalScrollBarPolicy" $ TEnum e_ScrollBarPolicy
    -- TODO verticalScrollBar
  , mkProp "verticalScrollBarPolicy" $ TEnum e_ScrollBarPolicy
  , mkProp "viewport" $ TPtr $ TObj c_QWidget
  ]
