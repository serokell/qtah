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

module Graphics.UI.Qtah.Internal.Interface.Widgets.QGridLayout (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  MethodApplicability (MConst, MNormal),
  Purity (Nonpure),
  Type (TBitspace, TConst, TEnum, TInt, TObj, TPtr, TRef, TVoid),
  addReqIncludes,
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
  mkProps,
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Internal.Flags (qtVersion)
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (bs_Alignment, e_Corner)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QLayout (c_QLayout)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QLayoutItem (c_QLayoutItem)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QGridLayout"]
  [ QtExport $ ExportClass c_QGridLayout ]

c_QGridLayout =
  addReqIncludes [includeStd "QGridLayout",
                  includeLocal "wrap_qgridlayout.hpp"] $
  makeClass (ident "QGridLayout") Nothing [c_QLayout]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  ] $
  collect
  [ just $ mkMethod' "addItem" "addItem" [TPtr $ TObj c_QLayoutItem, TInt, TInt] TVoid
  , just $ mkMethod' "addItem" "addItemWithSpan"
    [TPtr $ TObj c_QLayoutItem, TInt, TInt, TInt, TInt, TBitspace bs_Alignment] TVoid
  , just $ mkMethod' "addLayout" "addLayout" [TPtr $ TObj c_QLayout, TInt, TInt] TVoid
  , just $ mkMethod' "addLayout" "addLayoutWithSpan"
    [TPtr $ TObj c_QLayout, TInt, TInt, TInt, TInt, TBitspace bs_Alignment] TVoid
  , just $ mkMethod' "addWidget" "addWidget" [TPtr $ TObj c_QWidget, TInt, TInt] TVoid
  , just $ mkMethod' "addWidget" "addWidgetWithSpan"
    [TPtr $ TObj c_QWidget, TInt, TInt, TInt, TInt, TBitspace bs_Alignment] TVoid
  , just $ mkConstMethod "cellRect" [TInt, TInt] $ TObj c_QRect
  , just $ mkConstMethod "columnCount" [] TInt
  , just $ mkConstMethod "columnMinimumWidth" [TInt] TInt
  , just $ mkConstMethod "columnStretch" [TInt] TInt
  , just $ makeFnMethod (ident2 "qtah" "qgridlayout" "getItemRow") "getItemRow"
    getItemPositionAppl Nonpure [getItemPositionThis, TInt] TInt
  , just $ makeFnMethod (ident2 "qtah" "qgridlayout" "getItemColumn") "getItemColumn"
    getItemPositionAppl Nonpure [getItemPositionThis, TInt] TInt
  , just $ makeFnMethod (ident2 "qtah" "qgridlayout" "getItemRowSpan") "getItemRowSpan"
    getItemPositionAppl Nonpure [getItemPositionThis, TInt] TInt
  , just $ makeFnMethod (ident2 "qtah" "qgridlayout" "getItemColumnSpan") "getItemColumnSpan"
    getItemPositionAppl Nonpure [getItemPositionThis, TInt] TInt
  , test (qtVersion >= [4, 4]) $
    mkConstMethod "itemAtPosition" [TInt, TInt] $ TPtr $ TObj c_QLayoutItem
  , just $ mkConstMethod "rowCount" [] TInt
  , just $ mkConstMethod "rowMinimumHeight" [TInt] TInt
  , just $ mkConstMethod "rowStretch" [TInt] TInt
  , just $ mkMethod "setColumnMinimumWidth" [TInt, TInt] TVoid
  , just $ mkMethod "setColumnStretch" [TInt, TInt] TVoid
  , just $ mkMethod "setRowMinimumHeight" [TInt, TInt] TVoid
  , just $ mkMethod "setRowStretch" [TInt, TInt] TVoid
  , just $ mkConstMethod "spacing" [] TInt
  ] ++
  (mkProps . collect)
  [ test (qtVersion >= [4, 3]) $ mkProp "horizontalSpacing" TInt
  , test (qtVersion >= [4, 3]) $ mkProp "verticalSpacing" TInt
  , just $ mkProp "originCorner" $ TEnum e_Corner
  ]

  where (getItemPositionAppl, getItemPositionThis) =
          if qtVersion >= [5, 0]
          then (MConst, TRef $ TConst $ TObj c_QGridLayout)
          else (MNormal, TRef $ TObj c_QGridLayout)
