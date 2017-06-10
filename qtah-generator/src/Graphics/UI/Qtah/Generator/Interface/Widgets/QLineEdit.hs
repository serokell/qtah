-- This file is part of Qtah.
--
-- Copyright 2015-2017 The Qtah Authors.
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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QLineEdit (
  aModule,
  e_EchoMode,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportEnum, ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
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
  )
import Foreign.Hoppy.Generator.Types (bitspaceT, boolT, constT, enumT, intT, objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Interface.Core.QMargins (c_QMargins)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (bs_Alignment, e_CursorMoveStyle)
import Graphics.UI.Qtah.Generator.Interface.Gui.QValidator (c_QValidator)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  c_Listener,
  c_ListenerIntInt,
  c_ListenerQString,
  )
import Graphics.UI.Qtah.Generator.Interface.Widgets.QMenu (c_QMenu)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QLineEdit"] $
  QtExport (ExportClass c_QLineEdit) :
  map QtExportSignal signals ++
  [ QtExport $ ExportEnum e_EchoMode ]

c_QLineEdit =
  addReqIncludes [includeStd "QLineEdit"] $
  classSetEntityPrefix "" $
  makeClass (ident "QLineEdit") Nothing [c_QWidget]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , mkCtor "newWithText" [objT c_QString]
  , mkCtor "newWithTextAndParent" [objT c_QString, ptrT $ objT c_QWidget]
  , mkProp "alignment" $ bitspaceT bs_Alignment
  , mkMethod "backspace" [] voidT
  , mkMethod "clear" [] voidT
    -- TODO completer
  , mkConstMethod "copy" [] voidT
  , mkMethod "createStandardContextMenu" [] $ ptrT $ objT c_QMenu
  , mkMethod "cursorBackward" [boolT, intT] voidT
  , mkMethod "cursorForward" [boolT, intT] voidT
  , mkProp "cursorMoveStyle" $ enumT e_CursorMoveStyle
  , mkProp "cursorPosition" intT
  , mkMethod "cursorPositionAt" [objT c_QPoint] intT
  , mkMethod "cursorWordBackward" [boolT] voidT
  , mkMethod "cursorWordForward" [boolT] voidT
  , mkMethod "cut" [] voidT
  , mkMethod "del" [] voidT
  , mkMethod "deselect" [] voidT
  , mkConstMethod "displayText" [] $ objT c_QString
  , mkProp "dragEnabled" boolT
  , mkProp "echoMode" $ enumT e_EchoMode
  , mkMethod "end" [boolT] voidT
  , mkBoolHasProp "frame"
  , mkConstMethod "hasAcceptableInput" [] boolT
  , mkConstMethod "hasSelectedText" [] boolT
  , mkMethod "home" [boolT] voidT
  , mkProp "inputMask" $ objT c_QString
  , mkMethod "insert" [objT c_QString] voidT
  , mkConstMethod "isRedoAvailable" [] boolT
  , mkConstMethod "isUndoAvailable" [] boolT
  , mkProp "maxLength" intT
  , mkBoolIsProp "modified"
  , mkMethod "paste" [] voidT
  , mkProp "placeholderText" $ objT c_QString
  , mkBoolIsProp "readOnly"
  , mkMethod "redo" [] voidT
  , mkMethod "selectAll" [] voidT
  , mkConstMethod "selectedText" [] $ objT c_QString
  , mkConstMethod "selectionStart" [] intT
  , mkMethod "setSelection" [intT, intT] voidT
  , mkMethod' "setTextMargins" "setTextMargins" [objT c_QMargins] voidT
  , mkMethod' "setTextMargins" "setTextMarginsRaw" [intT, intT, intT, intT] voidT
  , mkProp "text" $ objT c_QString
  , mkConstMethod "textMargins" [] $ objT c_QMargins
  , mkMethod "undo" [] voidT
  , mkProp "validator" $ ptrT $ constT $ objT c_QValidator
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
  makeQtEnum (ident1 "QLineEdit" "EchoMode") [includeStd "QLineEdit"]
  [ (0, ["normal"])
  , (1, ["no", "echo"])
  , (2, ["password"])
  , (3, ["password", "echo", "on", "edit"])
  ]
