-- This file is part of Qtah.
--
-- Copyright 2015 Bryan Gardiner <bog@khumba.net>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License version 3
-- as published by the Free Software Foundation.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Graphics.UI.Qtah.Internal.Interface.Widgets.QLayout (
  hoppyModule,
  qtModule,
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
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QMargins (c_QMargins)
import Graphics.UI.Qtah.Internal.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Internal.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Internal.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (bs_Alignment)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QLayoutItem (c_QLayoutItem)
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

hoppyModule = makeHoppyModule "Widgets" "QLayout" qtModule

qtModule =
  makeQtModule "Widgets.QLayout"
  [ QtExport $ ExportClass c_QLayout
  , QtExport $ ExportEnum e_SizeConstraint
  ]

c_QLayout =
  addReqIncludes [includeStd "QLayout"] $
  makeClass (ident "QLayout") Nothing [c_QObject, c_QLayoutItem]
  [] $  -- Abstract.
  [ mkMethod "activate" [] TBool
  , mkMethod "addItem" [TPtr $ TObj c_QLayoutItem] TVoid
  , mkMethod "addWidget" [TPtr $ TObj c_QWidget] TVoid
  , mkStaticMethod "closestAcceptableSize" [TPtr $ TObj c_QWidget, TObj c_QSize] $ TObj c_QSize
  , mkConstMethod "contentsMargins" [] $ TObj c_QMargins
  , mkConstMethod "contentsRect" [] $ TObj c_QRect
  , mkConstMethod "count" [] TInt
  , mkConstMethod "indexOf" [TPtr $ TObj c_QWidget] TInt
  , mkConstMethod "itemAt" [TInt] $ TPtr $ TObj c_QLayoutItem
  , mkConstMethod "parentWidget" [] $ TPtr $ TObj c_QWidget
  , mkMethod "removeItem" [TPtr $ TObj c_QLayoutItem] TVoid
  , mkMethod "removeWidget" [TPtr $ TObj c_QWidget] TVoid
  , mkMethod' "setAlignment" "setAlignment" [TBitspace bs_Alignment] TVoid
  , mkMethod' "setAlignment" "setLayoutAlignment"
    [TPtr $ TObj c_QLayout, TBitspace bs_Alignment] TBool
  , mkMethod' "setAlignment" "setWidgetAlignment"
    [TPtr $ TObj c_QWidget, TBitspace bs_Alignment] TBool
  , mkMethod "setContentsMargins" [TObj c_QMargins] TVoid
  , mkMethod "takeAt" [TInt] $ TPtr $ TObj c_QLayoutItem
  , mkMethod "update" [] TVoid
  ] ++
  mkProps
  [ mkBoolIsProp "enabled"
  , mkProp "menuBar" $ TPtr $ TObj c_QWidget
  , mkProp "sizeConstraint" $ TEnum e_SizeConstraint
  , mkProp "spacing" TInt
  ]

e_SizeConstraint =
  makeQtEnum (ident1 "QLayout" "SizeConstraint")
  [ (0, ["set", "default", "size", "constraint"])
  , (1, ["set", "no", "constraint"])
  , (2, ["set", "minimum", "size"])
  , (3, ["set", "fixed", "size"])
  , (4, ["set", "maximum", "size"])
  , (5, ["set", "min", "and", "max", "size"])
  ]
