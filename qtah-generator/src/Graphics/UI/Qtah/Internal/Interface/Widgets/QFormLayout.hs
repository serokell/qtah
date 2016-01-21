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

module Graphics.UI.Qtah.Internal.Interface.Widgets.QFormLayout (
  aModule,
  minVersion,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass, ExportEnum),
  MethodApplicability (MConst),
  Purity (Nonpure),
  Type (TBitspace, TConst, TEnum, TInt, TObj, TPtr, TRef, TVoid),
  addReqIncludes,
  ident,
  ident1,
  ident2,
  includeLocal,
  includeStd,
  makeClass,
  makeFnMethod,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkMethod',
  mkProp,
  mkProps,
  )
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (bs_Alignment)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QLayout (c_QLayout)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QLayoutItem (c_QLayoutItem)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

minVersion :: [Int]
minVersion = [4, 4]

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QFormLayout"] $
  [ QtExport $ ExportClass c_QFormLayout
  , QtExport $ ExportEnum e_FieldGrowthPolicy
  , QtExport $ ExportEnum e_ItemRole
  , QtExport $ ExportEnum e_RowWrapPolicy
  ]

c_QFormLayout =
  addReqIncludes [includeStd "QFormLayout",
                  includeLocal "wrap_qformlayout.hpp"] $
  makeClass (ident "QFormLayout") Nothing [c_QLayout]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  ] $
  [ mkMethod' "addRow" "addRowWidgetWidget" [TPtr $ TObj c_QWidget, TPtr $ TObj c_QWidget] TVoid
  , mkMethod' "addRow" "addRowWidgetLayout" [TPtr $ TObj c_QWidget, TPtr $ TObj c_QLayout] TVoid
  , mkMethod' "addRow" "addRowStringWidget" [TObj c_QString, TPtr $ TObj c_QWidget] TVoid
  , mkMethod' "addRow" "addRowStringLayout" [TObj c_QString, TPtr $ TObj c_QLayout] TVoid
  , mkMethod' "addRow" "addRowWidget" [TPtr $ TObj c_QWidget] TVoid
  , mkMethod' "addRow" "addRowLayout" [TPtr $ TObj c_QLayout] TVoid
  , makeFnMethod (ident2 "qtah" "qformlayout" "getItemRow") "getItemRow" MConst Nonpure
    [TRef $ TConst $ TObj c_QFormLayout, TInt] TInt
  , makeFnMethod (ident2 "qtah" "qformlayout" "getItemRole") "getItemRole" MConst Nonpure
    [TRef $ TConst $ TObj c_QFormLayout, TInt] $ TEnum e_ItemRole
  , makeFnMethod (ident2 "qtah" "qformlayout" "getLayoutRow") "getLayoutRow" MConst Nonpure
    [TRef $ TConst $ TObj c_QFormLayout, TPtr $ TObj c_QLayout] TInt
  , makeFnMethod (ident2 "qtah" "qformlayout" "getLayoutRole") "getLayoutRole" MConst Nonpure
    [TRef $ TConst $ TObj c_QFormLayout, TPtr $ TObj c_QLayout] $ TEnum e_ItemRole
  , makeFnMethod (ident2 "qtah" "qformlayout" "getWidgetRow") "getWidgetRow" MConst Nonpure
    [TRef $ TConst $ TObj c_QFormLayout, TPtr $ TObj c_QWidget] TInt
  , makeFnMethod (ident2 "qtah" "qformlayout" "getWidgetRole") "getWidgetRole" MConst Nonpure
    [TRef $ TConst $ TObj c_QFormLayout, TPtr $ TObj c_QWidget] $ TEnum e_ItemRole
  , mkMethod' "insertRow" "insertRowWidgetWidget"
    [TInt, TPtr $ TObj c_QWidget, TPtr $ TObj c_QWidget] TVoid
  , mkMethod' "insertRow" "insertRowWidgetLayout"
    [TInt, TPtr $ TObj c_QWidget, TPtr $ TObj c_QLayout] TVoid
  , mkMethod' "insertRow" "insertRowStringWidget"
    [TInt, TObj c_QString, TPtr $ TObj c_QWidget] TVoid
  , mkMethod' "insertRow" "insertRowStringLayout"
    [TInt, TObj c_QString, TPtr $ TObj c_QLayout] TVoid
  , mkMethod' "insertRow" "insertRowWidget" [TInt, TPtr $ TObj c_QWidget] TVoid
  , mkMethod' "insertRow" "insertRowLayout" [TInt, TPtr $ TObj c_QLayout] TVoid
  , mkConstMethod "itemAt" [TInt, TEnum e_ItemRole] $ TPtr $ TObj c_QLayoutItem
  , mkConstMethod' "labelForField" "labelForFieldWidget"
    [TPtr $ TObj c_QWidget] $ TPtr $ TObj c_QWidget
  , mkConstMethod' "labelForField" "labelForFieldLayout"
    [TPtr $ TObj c_QLayout] $ TPtr $ TObj c_QWidget
  , mkConstMethod "rowCount" [] TInt
  , mkMethod "setItem" [TInt, TEnum e_ItemRole, TPtr $ TObj c_QLayoutItem] TVoid
  , mkMethod "setLayout" [TInt, TEnum e_ItemRole, TPtr $ TObj c_QLayout] TVoid
  , mkMethod "setWidget" [TInt, TEnum e_ItemRole, TPtr $ TObj c_QWidget] TVoid
  ] ++
  mkProps
  [ mkProp "fieldGrowthPolicy" $ TEnum e_FieldGrowthPolicy
  , mkProp "formAlignment" $ TBitspace bs_Alignment
  , mkProp "horizontalSpacing" TInt
  , mkProp "labelAlignment" $ TBitspace bs_Alignment
  , mkProp "rowWrapPolicy" $ TEnum e_RowWrapPolicy
  , mkProp "spacing" TInt
  , mkProp "verticalSpacing" TInt
  ]

e_FieldGrowthPolicy =
  makeQtEnum (ident1 "QFormLayout" "FieldGrowthPolicy") [includeStd "QFormLayout"]
  [ (0, ["fields", "stay", "at", "size", "hint"])
  , (1, ["expanding", "fields", "grow"])
  , (2, ["all", "non", "fixed", "fields", "grow"])
  ]

e_ItemRole =
  makeQtEnum (ident1 "QFormLayout" "ItemRole") [includeStd "QFormLayout"]
  [ (0, ["label", "role"])
  , (1, ["field", "role"])
  , (2, ["spanning", "role"])
  ]

e_RowWrapPolicy =
  makeQtEnum (ident1 "QFormLayout" "RowWrapPolicy") [includeStd "QFormLayout"]
  [ (0, ["dont", "wrap", "rows"])
  , (1, ["wrap", "long", "rows"])
  , (2, ["wrap", "all", "rows"])
  ]
