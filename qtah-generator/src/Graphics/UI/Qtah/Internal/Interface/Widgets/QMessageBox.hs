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

module Graphics.UI.Qtah.Internal.Interface.Widgets.QMessageBox (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportBitspace, ExportClass, ExportEnum),
  Type (TBitspace, TEnum, TObj, TObjToHeap, TPtr, TVoid),
  addReqIncludes,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkMethod',
  mkProp,
  mkProps,
  mkStaticMethod,
  mkStaticMethod',
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Internal.Flags (qtVersion)
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QList (c_QListQAbstractButton)
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (
  bs_TextInteractionFlags,
  e_TextFormat,
  e_WindowModality,
  )
import Graphics.UI.Qtah.Internal.Interface.Listener (c_ListenerPtrQAbstractButton)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QAbstractButton (c_QAbstractButton)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QCheckBox (c_QCheckBox)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QDialog (c_QDialog)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QPushButton (c_QPushButton)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

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
  makeClass (ident "QMessageBox") Nothing [c_QDialog]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  ] $
  collect
  [ just $ mkStaticMethod "about" [TPtr $ TObj c_QWidget, TObj c_QString, TObj c_QString] TVoid
  , just $ mkStaticMethod "aboutQt" [TPtr $ TObj c_QWidget, TObj c_QString] TVoid
  , test (qtVersion >= [4, 2]) $ mkMethod' "addButton" "addButton"
    [TPtr $ TObj c_QAbstractButton, TEnum e_ButtonRole] TVoid
  , test (qtVersion >= [4, 2]) $ mkMethod' "addButton" "addNewButton"
    [TObj c_QString, TEnum e_ButtonRole] $ TPtr $ TObj c_QPushButton
  , test (qtVersion >= [4, 2]) $ mkMethod' "addButton" "addStandardButton"
    [TEnum e_StandardButton] $ TPtr $ TObj c_QPushButton
  , test (qtVersion >= [4, 2]) $ mkConstMethod "button"
    [TEnum e_StandardButton] $ TPtr $ TObj c_QAbstractButton
  , test (qtVersion >= [4, 5]) $ mkConstMethod "buttonRole"
    [TPtr $ TObj c_QAbstractButton] $ TEnum e_ButtonRole
  , test (qtVersion >= [4, 5]) $ mkConstMethod' "buttons" "buttonsNew"
    [] $ TObjToHeap c_QListQAbstractButton
  , test (qtVersion >= [4, 2]) $ mkConstMethod "clickedButton" [] $ TPtr $ TObj c_QAbstractButton
  , test (qtVersion >= [4, 2]) $ mkStaticMethod' "critical" "critical"
    [TPtr $ TObj c_QWidget, TObj c_QString, TObj c_QString] $ TEnum e_StandardButton
  , test (qtVersion >= [4, 2]) $ mkStaticMethod' "critical" "criticalWithButtons"
    [TPtr $ TObj c_QWidget, TObj c_QString, TObj c_QString,
     TBitspace bs_StandardButtons, TEnum e_StandardButton] $
    TEnum e_StandardButton
  , just $ mkConstMethod "defaultButton" [] $ TPtr $ TObj c_QPushButton
  , test (qtVersion >= [4, 2]) $ mkConstMethod "escapeButton" [] $ TPtr $ TObj c_QAbstractButton
  , test (qtVersion >= [4, 2]) $ mkStaticMethod' "information" "information"
    [TPtr $ TObj c_QWidget, TObj c_QString, TObj c_QString] $ TEnum e_StandardButton
  , test (qtVersion >= [4, 2]) $ mkStaticMethod' "information" "informationWithButtons"
    [TPtr $ TObj c_QWidget, TObj c_QString, TObj c_QString,
     TBitspace bs_StandardButtons, TEnum e_StandardButton] $
    TEnum e_StandardButton
    -- OMIT open
  , test (qtVersion >= [4, 2]) $ mkStaticMethod' "question" "question"
    [TPtr $ TObj c_QWidget, TObj c_QString, TObj c_QString] $ TEnum e_StandardButton
  , test (qtVersion >= [4, 2]) $ mkStaticMethod' "question" "questionWithButtons"
    [TPtr $ TObj c_QWidget, TObj c_QString, TObj c_QString,
     TBitspace bs_StandardButtons, TEnum e_StandardButton] $
    TEnum e_StandardButton
  , test (qtVersion >= [4, 2]) $ mkMethod "removeButton" [TPtr $ TObj c_QAbstractButton] TVoid
  , test (qtVersion >= [4, 2]) $ mkMethod' "setDefaultButton" "setDefaultButton"
    [TPtr $ TObj c_QPushButton] TVoid
  , test (qtVersion >= [4, 3]) $ mkMethod' "setDefaultButton" "setDefaultButtonStandard"
    [TEnum e_StandardButton] TVoid
  , test (qtVersion >= [4, 2]) $ mkMethod' "setEscapeButton" "setEscapeButton"
    [TPtr $ TObj c_QPushButton] TVoid
  , test (qtVersion >= [4, 3]) $ mkMethod' "setEscapeButton" "setEscapeButtonStandard"
    [TEnum e_StandardButton] TVoid
  , test (qtVersion >= [4, 2]) $ mkMethod "setWindowModality" [TEnum e_WindowModality] TVoid
  , test (qtVersion >= [4, 2]) $ mkMethod "setWindowTitle" [TObj c_QString] TVoid
  , test (qtVersion >= [4, 2]) $ mkConstMethod "standardButton"
    [TPtr $ TObj c_QAbstractButton] $ TEnum e_StandardButton
  , test (qtVersion >= [4, 2]) $ mkStaticMethod' "warning" "warning"
    [TPtr $ TObj c_QWidget, TObj c_QString, TObj c_QString] $ TEnum e_StandardButton
  , test (qtVersion >= [4, 2]) $ mkStaticMethod' "warning" "warningWithButtons"
    [TPtr $ TObj c_QWidget, TObj c_QString, TObj c_QString,
     TBitspace bs_StandardButtons, TEnum e_StandardButton] $
    TEnum e_StandardButton
  ] ++
  (mkProps . collect)
  [ test (qtVersion >= [5, 2]) $ mkProp "checkBox" $ TPtr $ TObj c_QCheckBox
  , test (qtVersion >= [4, 2]) $ mkProp "detailedText" $ TObj c_QString
  , just $ mkProp "icon" $ TEnum e_Icon
    -- TODO iconPixmap
  , test (qtVersion >= [4, 2]) $ mkProp "informativeText" $ TObj c_QString
  , test (qtVersion >= [4, 2]) $ mkProp "standardButtons" $ TBitspace bs_StandardButtons
  , just $ mkProp "text" $ TObj c_QString
  , just $ mkProp "textFormat" $ TEnum e_TextFormat
  , test (qtVersion >= [5, 1]) $ mkProp "textInteractionFlags" $ TBitspace bs_TextInteractionFlags
  ]

signals =
  [ makeSignal c_QMessageBox "buttonClicked" c_ListenerPtrQAbstractButton
  ]

e_ButtonRole =
  addReqIncludes [includeStd "QMessageBox"] $
  makeQtEnum (ident1 "QMessageBox" "ButtonRole")
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
  addReqIncludes [includeStd "QMessageBox"] $
  makeQtEnum (ident1 "QMessageBox" "Icon")
  [ (0, ["no", "icon"])
  , (1, ["information"])
  , (2, ["warning"])
  , (3, ["critical"])
  , (4, ["question"])
  ]

(e_StandardButton, bs_StandardButtons) =
  makeQtEnumBitspace (ident1 "QMessageBox" "StandardButton") "StandardButtons"
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
