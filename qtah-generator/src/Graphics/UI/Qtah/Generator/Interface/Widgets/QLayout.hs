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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QLayout (
  aModule,
  c_QLayout,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportEnum, ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkBoolIsProp,
  mkConstMethod,
  mkMethod,
  mkMethod',
  mkStaticMethod,
  mkProp,
  )
import Foreign.Hoppy.Generator.Types (bitspaceT, boolT, enumT, intT, objT, ptrT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QMargins (c_QMargins)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (bs_Alignment)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QLayoutItem (c_QLayoutItem)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QLayout"]
  [ QtExport $ ExportClass c_QLayout
  , QtExport $ ExportEnum e_SizeConstraint
  ]

c_QLayout =
  addReqIncludes [includeStd "QLayout"] $
  classSetEntityPrefix "" $
  makeClass (ident "QLayout") Nothing [c_QObject, c_QLayoutItem] $
  collect
  -- Abstract.
  [ just $ mkMethod "activate" [] boolT
  , just $ mkMethod "addItem" [ptrT $ objT c_QLayoutItem] voidT
  , just $ mkMethod "addWidget" [ptrT $ objT c_QWidget] voidT
  , just $ mkStaticMethod "closestAcceptableSize"
    [ptrT $ objT c_QWidget, objT c_QSize] $ objT c_QSize
  , test (qtVersion >= [4, 6]) $ mkConstMethod "contentsMargins" [] $ objT c_QMargins
  , test (qtVersion >= [4, 3]) $ mkConstMethod "contentsRect" [] $ objT c_QRect
  , just $ mkConstMethod "count" [] intT
  , just $ mkBoolIsProp "enabled"
  , just $ mkConstMethod "indexOf" [ptrT $ objT c_QWidget] intT
  , just $ mkConstMethod "itemAt" [intT] $ ptrT $ objT c_QLayoutItem
  , just $ mkProp "menuBar" $ ptrT $ objT c_QWidget
  , just $ mkConstMethod "parentWidget" [] $ ptrT $ objT c_QWidget
  , just $ mkMethod "removeItem" [ptrT $ objT c_QLayoutItem] voidT
  , just $ mkMethod "removeWidget" [ptrT $ objT c_QWidget] voidT
  , just $ mkMethod' "setAlignment" "setAlignment" [bitspaceT bs_Alignment] voidT
  , just $ mkMethod' "setAlignment" "setLayoutAlignment"
    [ptrT $ objT c_QLayout, bitspaceT bs_Alignment] boolT
  , just $ mkMethod' "setAlignment" "setWidgetAlignment"
    [ptrT $ objT c_QWidget, bitspaceT bs_Alignment] boolT
  , test (qtVersion >= [4, 6]) $ mkMethod' "setContentsMargins" "setContentsMargins"
    [objT c_QMargins] voidT
  , test (qtVersion >= [4, 3]) $ mkMethod' "setContentsMargins" "setContentsMarginsRaw"
    [intT, intT, intT, intT] voidT
  , just $ mkProp "sizeConstraint" $ enumT e_SizeConstraint
  , just $ mkProp "spacing" intT
  , just $ mkMethod "takeAt" [intT] $ ptrT $ objT c_QLayoutItem
  , just $ mkMethod "update" [] voidT
  ]

e_SizeConstraint =
  makeQtEnum (ident1 "QLayout" "SizeConstraint") [includeStd "QLayout"]
  [ (0, ["set", "default", "size", "constraint"])
  , (1, ["set", "no", "constraint"])
  , (2, ["set", "minimum", "size"])
  , (3, ["set", "fixed", "size"])
  , (4, ["set", "maximum", "size"])
  , (5, ["set", "min", "and", "max", "size"])
  ]
