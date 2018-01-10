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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QMessageBox (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportBitspace, ExportClass, ExportEnum),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkMethod',
  mkProp,
  mkStaticMethod,
  mkStaticMethod',
  )
import Foreign.Hoppy.Generator.Types (bitspaceT, enumT, objT, ptrT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QList (c_QListQAbstractButton)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (
  bs_TextInteractionFlags,
  e_TextFormat,
  e_WindowModality,
  )
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (c_ListenerPtrQAbstractButton)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPixmap (c_QPixmap)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractButton (c_QAbstractButton)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QCheckBox (c_QCheckBox)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QDialog (c_QDialog)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QPushButton (c_QPushButton)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QMessageBox"] $
  QtExport (ExportClass c_QMessageBox) :
  map QtExportSignal signals ++
  [ QtExport $ ExportEnum e_ButtonRole
  , QtExport $ ExportEnum e_Icon
  , QtExport $ ExportEnum e_StandardButton
  , QtExport $ ExportBitspace bs_StandardButtons
  ]

c_QMessageBox =
  addReqIncludes [includeStd "QMessageBox"] $
  classSetEntityPrefix "" $
  makeClass (ident "QMessageBox") Nothing [c_QDialog] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , just $ mkStaticMethod "about" [ptrT $ objT c_QWidget, objT c_QString, objT c_QString] voidT
  , just $ mkStaticMethod "aboutQt" [ptrT $ objT c_QWidget, objT c_QString] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod' "addButton" "addButton"
    [ptrT $ objT c_QAbstractButton, enumT e_ButtonRole] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod' "addButton" "addNewButton"
    [objT c_QString, enumT e_ButtonRole] $ ptrT $ objT c_QPushButton
  , test (qtVersion >= [4, 2]) $ mkMethod' "addButton" "addStandardButton"
    [enumT e_StandardButton] $ ptrT $ objT c_QPushButton
  , test (qtVersion >= [4, 2]) $ mkConstMethod "button"
    [enumT e_StandardButton] $ ptrT $ objT c_QAbstractButton
  , test (qtVersion >= [4, 5]) $ mkConstMethod "buttonRole"
    [ptrT $ objT c_QAbstractButton] $ enumT e_ButtonRole
  , test (qtVersion >= [4, 5]) $ mkConstMethod "buttons" [] $ objT c_QListQAbstractButton
  , test (qtVersion >= [5, 2]) $ mkProp "checkBox" $ ptrT $ objT c_QCheckBox
  , test (qtVersion >= [4, 2]) $ mkConstMethod "clickedButton" [] $ ptrT $ objT c_QAbstractButton
  , test (qtVersion >= [4, 2]) $ mkStaticMethod' "critical" "critical"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString] $ enumT e_StandardButton
  , test (qtVersion >= [4, 2]) $ mkStaticMethod' "critical" "criticalWithButtons"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString,
     bitspaceT bs_StandardButtons, enumT e_StandardButton] $
    enumT e_StandardButton
  , just $ mkConstMethod "defaultButton" [] $ ptrT $ objT c_QPushButton
  , test (qtVersion >= [4, 2]) $ mkProp "detailedText" $ objT c_QString
  , test (qtVersion >= [4, 2]) $ mkConstMethod "escapeButton" [] $ ptrT $ objT c_QAbstractButton
  , just $ mkProp "icon" $ enumT e_Icon
  , just $ mkProp "iconPixmap" $ objT c_QPixmap
  , test (qtVersion >= [4, 2]) $ mkStaticMethod' "information" "information"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString] $ enumT e_StandardButton
  , test (qtVersion >= [4, 2]) $ mkStaticMethod' "information" "informationWithButtons"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString,
     bitspaceT bs_StandardButtons, enumT e_StandardButton] $
    enumT e_StandardButton
    -- OMIT open
  , test (qtVersion >= [4, 2]) $ mkProp "informativeText" $ objT c_QString
  , test (qtVersion >= [4, 2]) $ mkStaticMethod' "question" "question"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString] $ enumT e_StandardButton
  , test (qtVersion >= [4, 2]) $ mkStaticMethod' "question" "questionWithButtons"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString,
     bitspaceT bs_StandardButtons, enumT e_StandardButton] $
    enumT e_StandardButton
  , test (qtVersion >= [4, 2]) $ mkMethod "removeButton" [ptrT $ objT c_QAbstractButton] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod' "setDefaultButton" "setDefaultButton"
    [ptrT $ objT c_QPushButton] voidT
  , test (qtVersion >= [4, 3]) $ mkMethod' "setDefaultButton" "setDefaultButtonStandard"
    [enumT e_StandardButton] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod' "setEscapeButton" "setEscapeButton"
    [ptrT $ objT c_QPushButton] voidT
  , test (qtVersion >= [4, 3]) $ mkMethod' "setEscapeButton" "setEscapeButtonStandard"
    [enumT e_StandardButton] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod "setWindowModality" [enumT e_WindowModality] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod "setWindowTitle" [objT c_QString] voidT
  , test (qtVersion >= [4, 2]) $ mkConstMethod "standardButton"
    [ptrT $ objT c_QAbstractButton] $ enumT e_StandardButton
  , test (qtVersion >= [4, 2]) $ mkProp "standardButtons" $ bitspaceT bs_StandardButtons
  , just $ mkProp "text" $ objT c_QString
  , just $ mkProp "textFormat" $ enumT e_TextFormat
  , test (qtVersion >= [5, 1]) $ mkProp "textInteractionFlags" $ bitspaceT bs_TextInteractionFlags
  , test (qtVersion >= [4, 2]) $ mkStaticMethod' "warning" "warning"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString] $ enumT e_StandardButton
  , test (qtVersion >= [4, 2]) $ mkStaticMethod' "warning" "warningWithButtons"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString,
     bitspaceT bs_StandardButtons, enumT e_StandardButton] $
    enumT e_StandardButton
  ]

signals =
  [ makeSignal c_QMessageBox "buttonClicked" c_ListenerPtrQAbstractButton
  ]

e_ButtonRole =
  makeQtEnum (ident1 "QMessageBox" "ButtonRole") [includeStd "QMessageBox"]
  [ (-1, ["invalid", "role"])
  , (0, ["accept", "role"])
  , (1, ["reject", "role"])
  , (2, ["descructive", "role"])
  , (3, ["action", "role"])
  , (4, ["help", "role"])
  , (5, ["yes", "role"])
  , (6, ["no", "role"])
  , (7, ["apply", "role"])
  , (8, ["reset", "role"])
  ]

e_Icon =
  makeQtEnum (ident1 "QMessageBox" "Icon") [includeStd "QMessageBox"]
  [ (0, ["no", "icon"])
  , (1, ["information"])
  , (2, ["warning"])
  , (3, ["critical"])
  , (4, ["question"])
  ]

(e_StandardButton, bs_StandardButtons) =
  makeQtEnumBitspace (ident1 "QMessageBox" "StandardButton") "StandardButtons"
  [includeStd "QMessageBox"]
  [ (0x00000400, ["ok"])
  , (0x00002000, ["open"])
  , (0x00000800, ["save"])
  , (0x00400000, ["cancel"])
  , (0x00200000, ["close"])
  , (0x00800000, ["discard"])
  , (0x02000000, ["apply"])
  , (0x04000000, ["reset"])
  , (0x08000000, ["restore", "defaults"])
  , (0x01000000, ["help"])
  , (0x00001000, ["save", "all"])
  , (0x00004000, ["yes"])
  , (0x00008000, ["yes", "to", "all"])
  , (0x00010000, ["no"])
  , (0x00020000, ["no", "to", "all"])
  , (0x00040000, ["abort"])
  , (0x00080000, ["retry"])
  , (0x00100000, ["ignore"])
  , (0x00000000, ["no", "button"])
  ]
