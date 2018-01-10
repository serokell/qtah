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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QInputDialog (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportBitspace, ExportEnum, ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkBoolIsProp,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkProp,
  mkStaticMethod',
  )
import Foreign.Hoppy.Generator.Types (bitspaceT, boolT, doubleT, enumT, intT, objT, ptrT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QStringList (c_QStringList)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (bs_InputMethodHints, bs_WindowFlags)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  c_ListenerDouble,
  c_ListenerInt,
  c_ListenerQString,
  )
import Graphics.UI.Qtah.Generator.Interface.Widgets.QDialog (c_QDialog)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QLineEdit (e_EchoMode)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

minVersion = [4, 5]

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Widgets", "QInputDialog"] minVersion $
  QtExport (ExportClass c_QInputDialog) :
  map QtExportSignal signals ++
  [ QtExport $ ExportEnum e_InputDialogOption
  , QtExport $ ExportBitspace bs_InputDialogOptions
  , QtExport $ ExportEnum e_InputMode
  ]

c_QInputDialog =
  addReqIncludes [includeStd "QInputDialog"] $
  classSetEntityPrefix "" $
  makeClass (ident "QInputDialog") Nothing [c_QDialog] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , just $ mkCtor "newWithParentAndFlags" [ptrT $ objT c_QWidget, bitspaceT bs_WindowFlags]
  , just $ mkProp "cancelButtonText" $ objT c_QString
  , just $ mkBoolIsProp "comboBoxEditable"
  , just $ mkProp "comboBoxItems" $ objT c_QStringList
  , just $ mkProp "doubleDecimals" intT
  , just $ mkProp "doubleMaximum" doubleT
  , just $ mkProp "doubleMinimum" doubleT
  , just $ mkProp "doubleValue" doubleT
  , just $ mkStaticMethod' "getDouble" "getDouble"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString] doubleT
  , just $ mkStaticMethod' "getDouble" "getDoubleWithOptions"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString, doubleT, doubleT, doubleT,
     intT, ptrT boolT, bitspaceT bs_WindowFlags]
    doubleT
  , just $ mkStaticMethod' "getInt" "getInt"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString] intT
  , just $ mkStaticMethod' "getInt" "getIntWithOptions"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString, intT, intT, intT,
     intT, ptrT boolT, bitspaceT bs_WindowFlags]
    intT
  , just $ mkStaticMethod' "getItem" "getItem"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString, objT c_QStringList] $ objT c_QString
  , just $ mkStaticMethod' "getItem" "getItemWithOptions"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString, objT c_QStringList,
     intT, boolT, ptrT boolT, bitspaceT bs_WindowFlags, bitspaceT bs_InputMethodHints] $
    objT c_QString
  , test (qtVersion >= [5, 2]) $ mkStaticMethod' "getMultiLineText" "getMultiLineText"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString] $ objT c_QString
  , test (qtVersion >= [5, 2]) $ mkStaticMethod' "getMultiLineText" "getMultiLineTextWithOptions"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString, objT c_QString, ptrT boolT,
     bitspaceT bs_WindowFlags, bitspaceT bs_InputMethodHints] $
    objT c_QString
  , just $ mkStaticMethod' "getText" "getText"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString] $ objT c_QString
  , just $ mkStaticMethod' "getText" "getTextWithOptions"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString, enumT e_EchoMode,
     objT c_QString, ptrT boolT, bitspaceT bs_WindowFlags, bitspaceT bs_InputMethodHints] $
    objT c_QString
  , just $ mkProp "inputMode" $ enumT e_InputMode
  , just $ mkProp "intMaximum" intT
  , just $ mkProp "intMinimum" intT
  , just $ mkProp "intStep" intT
  , just $ mkProp "intValue" intT
  , just $ mkProp "labelText" $ objT c_QString
  , just $ mkProp "okButtonText" $ objT c_QString
    -- TODO open (if it can fit into the type system nicely)
  , just $ mkProp "options" $ bitspaceT bs_InputDialogOptions
  , just $ mkMethod "setDoubleRange" [doubleT, doubleT] voidT
  , just $ mkMethod "setIntRange" [intT, intT] voidT
  , just $ mkMethod "setOption" [enumT e_InputDialogOption, boolT] voidT
  , just $ mkConstMethod "testOption" [enumT e_InputDialogOption] boolT
  , just $ mkProp "textEchoMode" $ enumT e_EchoMode
  , just $ mkProp "textValue" $ objT c_QString
  ]

signals =
  [ makeSignal c_QInputDialog "doubleValueChanged" c_ListenerDouble
  , makeSignal c_QInputDialog "doubleValueSelected" c_ListenerDouble
  , makeSignal c_QInputDialog "intValueChanged" c_ListenerInt
  , makeSignal c_QInputDialog "intValueSelected" c_ListenerInt
  , makeSignal c_QInputDialog "textValueChanged" c_ListenerQString
  , makeSignal c_QInputDialog "textValueSelected" c_ListenerQString
  ]

(e_InputDialogOption, bs_InputDialogOptions) =
  makeQtEnumBitspace (ident1 "QInputDialog" "InputDialogOption") "InputDialogOptions"
  [includeStd "QInputDialog"]
  [ (0x1, ["no", "buttons"])
  , (0x2, ["use", "list", "view", "for", "combo", "box", "items"])
  , (0x4, ["use", "plain", "text", "edit", "for", "text", "input"])
  ]

e_InputMode =
  makeQtEnum (ident1 "QInputDialog" "InputMode") [includeStd "QInputDialog"]
  [ (0, ["text", "input"])
  , (1, ["int", "input"])
  , (2, ["double", "input"])
  ]
