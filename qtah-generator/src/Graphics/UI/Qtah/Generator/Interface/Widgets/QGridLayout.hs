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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QGridLayout (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  MethodApplicability (MConst, MNormal),
  Purity (Nonpure),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident2,
  includeLocal,
  includeStd,
  makeClass,
  makeFnMethod,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkMethod',
  mkProp,
  )
import Foreign.Hoppy.Generator.Types (bitspaceT, constT, enumT, intT, objT, ptrT, refT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (bs_Alignment, e_Corner)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QLayout (c_QLayout)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QLayoutItem (c_QLayoutItem)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QGridLayout"]
  [ QtExport $ ExportClass c_QGridLayout ]

c_QGridLayout =
  addReqIncludes [includeStd "QGridLayout",
                  includeLocal "wrap_qgridlayout.hpp"] $
  classSetEntityPrefix "" $
  makeClass (ident "QGridLayout") Nothing [c_QLayout] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , just $ mkMethod' "addItem" "addItem" [ptrT $ objT c_QLayoutItem, intT, intT] voidT
  , just $ mkMethod' "addItem" "addItemWithSpan"
    [ptrT $ objT c_QLayoutItem, intT, intT, intT, intT] voidT
  , just $ mkMethod' "addItem" "addItemWithSpanAndAlignment"
    [ptrT $ objT c_QLayoutItem, intT, intT, intT, intT, bitspaceT bs_Alignment] voidT
  , just $ mkMethod' "addLayout" "addLayout" [ptrT $ objT c_QLayout, intT, intT] voidT
  , just $ mkMethod' "addLayout" "addLayoutWithSpan"
    [ptrT $ objT c_QLayout, intT, intT, intT, intT] voidT
  , just $ mkMethod' "addLayout" "addLayoutWithSpanAndAlignment"
    [ptrT $ objT c_QLayout, intT, intT, intT, intT, bitspaceT bs_Alignment] voidT
  , just $ mkMethod' "addWidget" "addWidget" [ptrT $ objT c_QWidget, intT, intT] voidT
  , just $ mkMethod' "addWidget" "addWidgetWithSpan"
    [ptrT $ objT c_QWidget, intT, intT, intT, intT] voidT
  , just $ mkMethod' "addWidget" "addWidgetWithSpanAndAlignment"
    [ptrT $ objT c_QWidget, intT, intT, intT, intT, bitspaceT bs_Alignment] voidT
  , just $ mkConstMethod "cellRect" [intT, intT] $ objT c_QRect
  , just $ mkConstMethod "columnCount" [] intT
  , just $ mkConstMethod "columnMinimumWidth" [intT] intT
  , just $ mkConstMethod "columnStretch" [intT] intT
  , just $ makeFnMethod (ident2 "qtah" "qgridlayout" "getItemRow") "getItemRow"
    getItemPositionAppl Nonpure [getItemPositionThis, intT] intT
  , just $ makeFnMethod (ident2 "qtah" "qgridlayout" "getItemColumn") "getItemColumn"
    getItemPositionAppl Nonpure [getItemPositionThis, intT] intT
  , just $ makeFnMethod (ident2 "qtah" "qgridlayout" "getItemRowSpan") "getItemRowSpan"
    getItemPositionAppl Nonpure [getItemPositionThis, intT] intT
  , just $ makeFnMethod (ident2 "qtah" "qgridlayout" "getItemColumnSpan") "getItemColumnSpan"
    getItemPositionAppl Nonpure [getItemPositionThis, intT] intT
  , test (qtVersion >= [4, 3]) $ mkProp "horizontalSpacing" intT
  , test (qtVersion >= [4, 4]) $
    mkConstMethod "itemAtPosition" [intT, intT] $ ptrT $ objT c_QLayoutItem
  , just $ mkProp "originCorner" $ enumT e_Corner
  , just $ mkConstMethod "rowCount" [] intT
  , just $ mkConstMethod "rowMinimumHeight" [intT] intT
  , just $ mkConstMethod "rowStretch" [intT] intT
  , just $ mkMethod "setColumnMinimumWidth" [intT, intT] voidT
  , just $ mkMethod "setColumnStretch" [intT, intT] voidT
  , just $ mkMethod "setRowMinimumHeight" [intT, intT] voidT
  , just $ mkMethod "setRowStretch" [intT, intT] voidT
  , just $ mkConstMethod "spacing" [] intT
  , test (qtVersion >= [4, 3]) $ mkProp "verticalSpacing" intT
  ]

  where (getItemPositionAppl, getItemPositionThis) =
          if qtVersion >= [5, 0]
          then (MConst, refT $ constT $ objT c_QGridLayout)
          else (MNormal, refT $ objT c_QGridLayout)
