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

module Graphics.UI.Qtah.Internal.Interface.Widgets.QDialogButtonBox (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportBitspace, ExportClass, ExportEnum),
  Type (TBitspace, TBool, TEnum, TObj, TPtr, TVoid),
  addReqIncludes,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkMethod',
  mkProp,
  mkProps,
  )
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QList (c_QListQAbstractButton)
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (e_Orientation)
import Graphics.UI.Qtah.Internal.Interface.Listener (
  c_Listener,
  c_ListenerPtrQAbstractButton,
  )
import Graphics.UI.Qtah.Internal.Interface.Widgets.QAbstractButton (c_QAbstractButton)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QPushButton (c_QPushButton)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

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
  makeClass (ident "QDialogButtonBox") Nothing [c_QWidget]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
    -- TODO Other ctors? v v
  --, mkCtor "newWithOrientation" [TEnum e_Orientation]
  --, mkCtor "newWithOrientationAndParent" [TEnum e_Orientation, TPtr $ TObj c_QWidget]
  --, mkCtor "newWithButtons" []
  --, mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  --, mkCtor "new" []
  --, mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  ] $
  [ mkMethod' "addButton" "addButton" [TPtr $ TObj c_QAbstractButton, TEnum e_ButtonRole] TVoid
  , mkMethod' "addButton" "addButtonWithText"
    [TObj c_QString, TEnum e_ButtonRole] $ TPtr $ TObj c_QPushButton
  , mkMethod' "addButton" "addStandardButton" [TEnum e_StandardButton] $ TPtr $ TObj c_QPushButton
  , mkConstMethod "button" [TEnum e_StandardButton] $ TPtr $ TObj c_QPushButton
  , mkConstMethod "buttonRole" [TPtr $ TObj c_QAbstractButton] $ TEnum e_ButtonRole
  , mkConstMethod "buttons" [] $ TObj c_QListQAbstractButton
  , mkMethod "clear" [] TVoid
  , mkMethod "removeButton" [TPtr $ TObj c_QAbstractButton] TVoid
  , mkConstMethod "standardButton" [TPtr $ TObj c_QAbstractButton] $ TEnum e_StandardButton
  ] ++
  mkProps
  [ mkProp "centerButtons" TBool
  , mkProp "orientation" $ TEnum e_Orientation
  , mkProp "standardButtons" $ TBitspace bs_StandardButtons
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
