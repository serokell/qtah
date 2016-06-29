-- This file is part of Qtah.
--
-- Copyright 2015-2016 Bryan Gardiner <bog@khumba.net>
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

module Graphics.UI.Qtah.Internal.Interface.Widgets.QLayout (
  aModule,
  c_QLayout,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportEnum, ExportClass),
  Type (TBitspace, TBool, TEnum, TInt, TObj, TPtr, TVoid),
  addReqIncludes,
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
  mkProps,
  )
import Graphics.UI.Qtah.Internal.Flag (collect, just, test)
import Graphics.UI.Qtah.Internal.Flags (qtVersion)
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QMargins (c_QMargins)
import Graphics.UI.Qtah.Internal.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Internal.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Internal.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (bs_Alignment)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QLayoutItem (c_QLayoutItem)
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QLayout"]
  [ QtExport $ ExportClass c_QLayout
  , QtExport $ ExportEnum e_SizeConstraint
  ]

c_QLayout =
  addReqIncludes [includeStd "QLayout"] $
  makeClass (ident "QLayout") Nothing [c_QObject, c_QLayoutItem]
  [] $  -- Abstract.
  collect
  [ just $ mkMethod "activate" [] TBool
  , just $ mkMethod "addItem" [TPtr $ TObj c_QLayoutItem] TVoid
  , just $ mkMethod "addWidget" [TPtr $ TObj c_QWidget] TVoid
  , just $ mkStaticMethod "closestAcceptableSize"
    [TPtr $ TObj c_QWidget, TObj c_QSize] $ TObj c_QSize
  , test (qtVersion >= [4, 6]) $ mkConstMethod "contentsMargins" [] $ TObj c_QMargins
  , test (qtVersion >= [4, 3]) $ mkConstMethod "contentsRect" [] $ TObj c_QRect
  , just $ mkConstMethod "count" [] TInt
  , just $ mkConstMethod "indexOf" [TPtr $ TObj c_QWidget] TInt
  , just $ mkConstMethod "itemAt" [TInt] $ TPtr $ TObj c_QLayoutItem
  , just $ mkConstMethod "parentWidget" [] $ TPtr $ TObj c_QWidget
  , just $ mkMethod "removeItem" [TPtr $ TObj c_QLayoutItem] TVoid
  , just $ mkMethod "removeWidget" [TPtr $ TObj c_QWidget] TVoid
  , just $ mkMethod' "setAlignment" "setAlignment" [TBitspace bs_Alignment] TVoid
  , just $ mkMethod' "setAlignment" "setLayoutAlignment"
    [TPtr $ TObj c_QLayout, TBitspace bs_Alignment] TBool
  , just $ mkMethod' "setAlignment" "setWidgetAlignment"
    [TPtr $ TObj c_QWidget, TBitspace bs_Alignment] TBool
  , test (qtVersion >= [4, 6]) $ mkMethod' "setContentsMargins" "setContentsMargins"
    [TObj c_QMargins] TVoid
  , test (qtVersion >= [4, 3]) $ mkMethod' "setContentsMargins" "setContentsMarginsRaw"
    [TInt, TInt, TInt, TInt] TVoid
  , just $ mkMethod "takeAt" [TInt] $ TPtr $ TObj c_QLayoutItem
  , just $ mkMethod "update" [] TVoid
  ] ++
  mkProps
  [ mkBoolIsProp "enabled"
  , mkProp "menuBar" $ TPtr $ TObj c_QWidget
  , mkProp "sizeConstraint" $ TEnum e_SizeConstraint
  , mkProp "spacing" TInt
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
