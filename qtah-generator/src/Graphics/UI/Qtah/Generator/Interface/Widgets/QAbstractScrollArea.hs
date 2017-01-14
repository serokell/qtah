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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractScrollArea (
  aModule,
  c_QAbstractScrollArea,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkProp,
  )
import Foreign.Hoppy.Generator.Types (bitspaceT, enumT, objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (bs_Alignment, e_ScrollBarPolicy)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QAbstractScrollArea"]
  [ QtExport $ ExportClass c_QAbstractScrollArea ]

c_QAbstractScrollArea =
  addReqIncludes [includeStd "QAbstractScrollArea"] $
  classSetEntityPrefix "" $
  makeClass (ident "QAbstractScrollArea") Nothing [c_QWidget]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , mkMethod "addScrollBarWidget" [ptrT $ objT c_QWidget, bitspaceT bs_Alignment] voidT
  , mkProp "cornerWidget" $ ptrT $ objT c_QWidget
    -- TODO horizontalScrollBar
  , mkProp "horizontalScrollBarPolicy" $ enumT e_ScrollBarPolicy
  , mkConstMethod "maximumViewportSize" [] $ objT c_QSize
    -- TODO scrollBarWidgets
    -- TODO verticalScrollBar
  , mkProp "verticalScrollBarPolicy" $ enumT e_ScrollBarPolicy
  , mkProp "viewport" $ ptrT $ objT c_QWidget
  ]
