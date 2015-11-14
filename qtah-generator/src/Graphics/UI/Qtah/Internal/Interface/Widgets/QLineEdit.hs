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

module Graphics.UI.Qtah.Internal.Interface.Widgets.QLineEdit (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportEnum, ExportClass),
  Type (TBitspace, TBool, TEnum, TInt, TObj, TPtr, TVoid),
  addReqIncludes,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkBoolHasProp,
  mkBoolIsProp,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkMethod',
  mkProp,
  mkProps,
  )
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QMargins (c_QMargins)
import Graphics.UI.Qtah.Internal.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (bs_Alignment, e_CursorMoveStyle)
import Graphics.UI.Qtah.Internal.Interface.Listener (
  c_Listener,
  c_ListenerIntInt,
  c_ListenerQString,
  )
import Graphics.UI.Qtah.Internal.Interface.Widgets.QMenu (c_QMenu)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QLineEdit"] $
  QtExport (ExportClass c_QLineEdit) :
  map QtExportSignal signals ++
  [ QtExport $ ExportEnum e_EchoMode ]

c_QLineEdit =
  addReqIncludes [includeStd "QLineEdit"] $
  makeClass (ident "QLineEdit") Nothing [c_QWidget]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  , mkCtor "newWithText" [TObj c_QString]
  , mkCtor "newWithTextAndParent" [TObj c_QString, TPtr $ TObj c_QWidget]
  ] $
  [ mkMethod "backspace" [] TVoid
  , mkMethod "clear" [] TVoid
  , mkConstMethod "copy" [] TVoid
  , mkMethod "createStandardContextMenu" [] $ TPtr $ TObj c_QMenu
  , mkMethod "cursorBackward" [TBool, TInt] TVoid
  , mkMethod "cursorForward" [TBool, TInt] TVoid
  , mkMethod "cursorPositionAt" [TObj c_QPoint] TInt
  , mkMethod "cursorWordBackward" [TBool] TVoid
  , mkMethod "cursorWordForward" [TBool] TVoid
  , mkMethod "cut" [] TVoid
  , mkMethod "del" [] TVoid
  , mkMethod "deselect" [] TVoid
  , mkConstMethod "displayText" [] $ TObj c_QString
  , mkMethod "end" [TBool] TVoid
  , mkConstMethod "hasAcceptableInput" [] TBool
  , mkConstMethod "hasSelectedText" [] TBool
  , mkMethod "home" [TBool] TVoid
  , mkMethod "insert" [TObj c_QString] TVoid
  , mkConstMethod "isRedoAvailable" [] TBool
  , mkConstMethod "isUndoAvailable" [] TBool
  , mkMethod "paste" [] TVoid
  , mkMethod "redo" [] TVoid
  , mkMethod "selectAll" [] TVoid
  , mkConstMethod "selectedText" [] $ TObj c_QString
  , mkConstMethod "selectionStart" [] TInt
  , mkMethod "setSelection" [TInt, TInt] TVoid
  , mkMethod' "setTextMargins" "setTextMargins" [TObj c_QMargins] TVoid
  , mkMethod' "setTextMargins" "setTextMarginsRaw" [TInt, TInt, TInt, TInt] TVoid
  , mkConstMethod "textMargins" [] $ TObj c_QMargins
  , mkMethod "undo" [] TVoid
  ] ++
  mkProps
  [ mkProp "alignment" $ TBitspace bs_Alignment
    -- TODO completer
  , mkProp "cursorMoveStyle" $ TEnum e_CursorMoveStyle
  , mkProp "cursorPosition" TInt
  , mkProp "dragEnabled" TBool
  , mkProp "echoMode" $ TEnum e_EchoMode
  , mkBoolHasProp "frame"
  , mkProp "inputMask" $ TObj c_QString
  , mkProp "maxLength" TInt
  , mkBoolIsProp "modified"
  , mkProp "placeholderText" $ TObj c_QString
  , mkBoolIsProp "readOnly"
  , mkProp "text" $ TObj c_QString
    -- TODO validator
  ]

signals =
  [ makeSignal c_QLineEdit "cursorPositionChanged" c_ListenerIntInt
  , makeSignal c_QLineEdit "editingFinished" c_Listener
  , makeSignal c_QLineEdit "returnPressed" c_Listener
  , makeSignal c_QLineEdit "selectionChanged" c_Listener
  , makeSignal c_QLineEdit "textEdited" c_ListenerQString
  , makeSignal c_QLineEdit "textChanged" c_ListenerQString
  ]

e_EchoMode =
  makeQtEnum (ident1 "QLineEdit" "EchoMode")
  [ (0, ["normal"])
  , (1, ["no", "echo"])
  , (2, ["password"])
  , (3, ["password", "echo", "on", "edit"])
  ]
