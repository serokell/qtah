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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QDialogButtonBox (
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
  )
import Foreign.Hoppy.Generator.Types (bitspaceT, boolT, enumT, objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Interface.Core.QList (c_QListQAbstractButton)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_Orientation)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  c_Listener,
  c_ListenerPtrQAbstractButton,
  )
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractButton (c_QAbstractButton)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QPushButton (c_QPushButton)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

minVersion = [4, 2]

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Widgets", "QDialogButtonBox"] minVersion $
  (QtExport $ ExportClass c_QDialogButtonBox) :
  map QtExportSignal signals ++
  map QtExport
  [ ExportEnum e_ButtonLayout
  , ExportEnum e_ButtonRole
  , ExportEnum e_StandardButton
  , ExportBitspace bs_StandardButtons
  ]

c_QDialogButtonBox =
  addReqIncludes [includeStd "QDialogButtonBox"] $
  classSetEntityPrefix "" $
  makeClass (ident "QDialogButtonBox") Nothing [c_QWidget]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [ptrT $ objT c_QWidget]
    -- TODO Other ctors? v v
  --, mkCtor "newWithOrientation" [enumT e_Orientation]
  --, mkCtor "newWithOrientationAndParent" [enumT e_Orientation, ptrT $ objT c_QWidget]
  --, mkCtor "newWithButtons" []
  --, mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  --, mkCtor "new" []
  --, mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , mkMethod' "addButton" "addButton" [ptrT $ objT c_QAbstractButton, enumT e_ButtonRole] voidT
  , mkMethod' "addButton" "addButtonWithText"
    [objT c_QString, enumT e_ButtonRole] $ ptrT $ objT c_QPushButton
  , mkMethod' "addButton" "addStandardButton" [enumT e_StandardButton] $ ptrT $ objT c_QPushButton
  , mkConstMethod "button" [enumT e_StandardButton] $ ptrT $ objT c_QPushButton
  , mkConstMethod "buttonRole" [ptrT $ objT c_QAbstractButton] $ enumT e_ButtonRole
  , mkConstMethod "buttons" [] $ objT c_QListQAbstractButton
  , mkProp "centerButtons" boolT
  , mkMethod "clear" [] voidT
  , mkProp "orientation" $ enumT e_Orientation
  , mkMethod "removeButton" [ptrT $ objT c_QAbstractButton] voidT
  , mkConstMethod "standardButton" [ptrT $ objT c_QAbstractButton] $ enumT e_StandardButton
  , mkProp "standardButtons" $ bitspaceT bs_StandardButtons
  ]

signals =
  [ makeSignal c_QDialogButtonBox "accepted" c_Listener
  , makeSignal c_QDialogButtonBox "clicked" c_ListenerPtrQAbstractButton
  , makeSignal c_QDialogButtonBox "helpRequested" c_Listener
  , makeSignal c_QDialogButtonBox "rejected" c_Listener
  ]

e_ButtonLayout =
  makeQtEnum (ident1 "QDialogButtonBox" "ButtonLayout") [includeStd "QDialogButtonBox"]
  [ (0, ["win", "layout"])
  , (1, ["mac", "layout"])
  , (2, ["kde", "layout"])
  , (3, ["gnome", "layout"])
  ]

e_ButtonRole =
  makeQtEnum (ident1 "QDialogButtonBox" "ButtonRole") [includeStd "QDialogButtonBox"]
  [ (-1, ["invalid", "role"])
  , (0, ["accept", "role"])
  , (1, ["reject", "role"])
  , (2, ["destructive", "role"])
  , (3, ["action", "role"])
  , (4, ["help", "role"])
  , (5, ["yes", "role"])
  , (6, ["no", "role"])
  , (7, ["reset", "role"])
  , (8, ["apply", "role"])
  ]

(e_StandardButton, bs_StandardButtons) =
  makeQtEnumBitspace (ident1 "QDialogButtonBox" "StandardButton") "StandardButtons"
  [includeStd "QDialogButtonBox"]
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
