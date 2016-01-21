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

module Graphics.UI.Qtah.Internal.Interface.Widgets.QBoxLayout (
  aModule,
  c_QBoxLayout,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportEnum, ExportClass),
  Type (TBitspace, TBool, TEnum, TInt, TObj, TPtr, TVoid),
  addReqIncludes,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkCtor,
  mkMethod,
  mkMethod',
  mkProp,
  mkProps,
  )
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.Types (bs_Alignment)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QLayout (c_QLayout)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QBoxLayout"]
  [ QtExport $ ExportClass c_QBoxLayout
  , QtExport $ ExportEnum e_Direction
  ]

c_QBoxLayout =
  addReqIncludes [includeStd "QBoxLayout"] $
  makeClass (ident "QBoxLayout") Nothing [c_QLayout]
  [ mkCtor "new" [TEnum e_Direction]
  , mkCtor "newWithParent" [TEnum e_Direction, TPtr $ TObj c_QWidget]
  ] $
  [ mkMethod' "addLayout" "addLayout" [TPtr $ TObj c_QLayout] TVoid
  , mkMethod' "addLayout" "addLayoutWithStretch" [TPtr $ TObj c_QLayout, TInt] TVoid
  , mkMethod "addSpacing" [TInt] TVoid
  , mkMethod' "addStretch" "addStretch" [] TVoid
  , mkMethod' "addStretch" "addStretchOf" [TInt] TVoid
  , mkMethod "addStrut" [TInt] TVoid
  , mkMethod' "addWidget" "addWidget" [TPtr $ TObj c_QWidget] TVoid
  , mkMethod' "addWidget" "addWidgetWithStretch" [TPtr $ TObj c_QWidget, TInt] TVoid
  , mkMethod' "addWidget" "addWidgetWithStretchAndAlignment"
    [TPtr $ TObj c_QWidget, TInt, TBitspace bs_Alignment] TVoid
  , mkMethod' "insertLayout" "insertLayout" [TInt, TPtr $ TObj c_QLayout] TVoid
  , mkMethod' "insertLayout" "insertLayoutWithStretch" [TInt, TPtr $ TObj c_QLayout, TInt] TVoid
    -- TODO insertSpacerItem
  , mkMethod "insertSpacing" [TInt, TInt] TVoid
  , mkMethod' "insertStretch" "insertStretch" [TInt] TVoid
  , mkMethod' "insertStretch" "insertStretchOf" [TInt, TInt] TVoid
  , mkMethod' "insertWidget" "insertWidget" [TInt, TPtr $ TObj c_QWidget] TVoid
  , mkMethod' "insertWidget" "insertWidgetWithStretch" [TInt, TPtr $ TObj c_QWidget, TInt] TVoid
  , mkMethod' "insertWidget" "insertWidgetWithStretchAndAlignment"
    [TInt, TPtr $ TObj c_QWidget, TInt, TBitspace bs_Alignment] TVoid
  , mkMethod "setStretch" [TInt, TInt] TVoid
  , mkMethod' "setStretchFactor" "setWidgetStretchFactor" [TPtr $ TObj c_QWidget, TInt] TBool
  , mkMethod' "setStretchFactor" "setLayoutStretchFactor" [TPtr $ TObj c_QLayout, TInt] TBool
  ] ++
  mkProps
  [ mkProp "direction" $ TEnum e_Direction
  , mkProp "spacing" TInt
  ]

e_Direction =
  makeQtEnum (ident1 "QBoxLayout" "Direction") [includeStd "QBoxLayout"]
  [ (0, ["left", "to", "right"])
  , (1, ["right", "to", "left"])
  , (2, ["top", "to", "bottom"])
  , (3, ["bottom", "to", "top"])
  ]
