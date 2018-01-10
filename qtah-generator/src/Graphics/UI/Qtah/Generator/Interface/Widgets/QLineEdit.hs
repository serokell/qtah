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
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
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
  makeClass (ident "QLineEdit") Nothing [c_QWidget] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , just $ mkCtor "newWithText" [objT c_QString]
  , just $ mkCtor "newWithTextAndParent" [objT c_QString, ptrT $ objT c_QWidget]
  , just $ mkProp "alignment" $ bitspaceT bs_Alignment
  , just $ mkMethod "backspace" [] voidT
  , just $ mkMethod "clear" [] voidT
  , test (qtVersion >= [5, 2]) $ mkBoolIsProp "clearButtonEnabled"
    -- TODO completer
  , just $ mkConstMethod "copy" [] voidT
  , just $ mkMethod "createStandardContextMenu" [] $ ptrT $ objT c_QMenu
  , just $ mkMethod "cursorBackward" [boolT, intT] voidT
  , just $ mkMethod "cursorForward" [boolT, intT] voidT
  , just $ mkProp "cursorMoveStyle" $ enumT e_CursorMoveStyle
  , just $ mkProp "cursorPosition" intT
  , just $ mkMethod "cursorPositionAt" [objT c_QPoint] intT
  , just $ mkMethod "cursorWordBackward" [boolT] voidT
  , just $ mkMethod "cursorWordForward" [boolT] voidT
  , just $ mkMethod "cut" [] voidT
  , just $ mkMethod "del" [] voidT
  , just $ mkMethod "deselect" [] voidT
  , just $ mkConstMethod "displayText" [] $ objT c_QString
  , just $ mkProp "dragEnabled" boolT
  , just $ mkProp "echoMode" $ enumT e_EchoMode
  , just $ mkMethod "end" [boolT] voidT
  , just $ mkBoolHasProp "frame"
  , just $ mkConstMethod "hasAcceptableInput" [] boolT
  , just $ mkConstMethod "hasSelectedText" [] boolT
  , just $ mkMethod "home" [boolT] voidT
  , just $ mkProp "inputMask" $ objT c_QString
  , just $ mkMethod "insert" [objT c_QString] voidT
  , just $ mkConstMethod "isRedoAvailable" [] boolT
  , just $ mkConstMethod "isUndoAvailable" [] boolT
  , just $ mkProp "maxLength" intT
  , just $ mkBoolIsProp "modified"
  , just $ mkMethod "paste" [] voidT
  , just $ mkProp "placeholderText" $ objT c_QString
  , just $ mkBoolIsProp "readOnly"
  , just $ mkMethod "redo" [] voidT
  , just $ mkMethod "selectAll" [] voidT
  , just $ mkConstMethod "selectedText" [] $ objT c_QString
  , just $ mkConstMethod "selectionStart" [] intT
  , just $ mkMethod "setSelection" [intT, intT] voidT
  , just $ mkMethod' "setTextMargins" "setTextMargins" [objT c_QMargins] voidT
  , just $ mkMethod' "setTextMargins" "setTextMarginsRaw" [intT, intT, intT, intT] voidT
  , just $ mkProp "text" $ objT c_QString
  , just $ mkConstMethod "textMargins" [] $ objT c_QMargins
  , just $ mkMethod "undo" [] voidT
  , just $ mkProp "validator" $ ptrT $ constT $ objT c_QValidator
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
