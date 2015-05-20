{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Widgets.QLineEdit (
  qtModule,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QMargins (c_QMargins)
import Graphics.UI.Qtah.Internal.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (e_Alignment, e_CursorMoveStyle)
import Graphics.UI.Qtah.Internal.Interface.Listener
import Graphics.UI.Qtah.Internal.Interface.Widgets.QMenu (c_QMenu)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)
#include "../Mk.hs.inc"

qtModule =
  makeQtModule "Widgets.QLineEdit" $
  QtExport (ExportClass c_QLineEdit) :
  map QtExportSignal signals ++
  [ QtExport $ ExportEnum e_EchoMode ]

this = c_QLineEdit

c_QLineEdit =
  addReqIncludes [includeStd "QLineEdit"] $
  makeClass (ident "QLineEdit") Nothing [c_QWidget]
  [ _mkCtor "new" []
  , _mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  , _mkCtor "newWithText" [TObj c_QString]
  , _mkCtor "newWithTextAndParent" [TObj c_QString, TPtr $ TObj c_QWidget]
  ] $
  [ _mkMethod "backspace" [] TVoid
  , _mkMethod "clear" [] TVoid
  , _mkConstMethod "copy" [] TVoid
  , _mkMethod "createStandardContextMenu" [] $ TPtr $ TObj c_QMenu
  , _mkMethod "cursorBackward" [TBool, TInt] TVoid
  , _mkMethod "cursorForward" [TBool, TInt] TVoid
  , _mkMethod "cursorPositionAt" [TObj c_QPoint] TInt
  , _mkMethod "cursorWordBackward" [TBool] TVoid
  , _mkMethod "cursorWordForward" [TBool] TVoid
  , _mkMethod "cut" [] TVoid
  , _mkMethod "del" [] TVoid
  , _mkMethod "deselect" [] TVoid
  , _mkConstMethod "displayText" [] $ TObj c_QString
  , _mkMethod "end" [TBool] TVoid
  , _mkConstMethod "hasAcceptableInput" [] TBool
  , _mkConstMethod "hasSelectedText" [] TBool
  , _mkMethod "home" [TBool] TVoid
  , _mkMethod "insert" [TObj c_QString] TVoid
  , _mkConstMethod "isRedoAvailable" [] TBool
  , _mkConstMethod "isUndoAvailable" [] TBool
  , _mkMethod "paste" [] TVoid
  , _mkMethod "redo" [] TVoid
  , _mkMethod "selectAll" [] TVoid
  , _mkConstMethod "selectedText" [] $ TObj c_QString
  , _mkConstMethod "selectionStart" [] TInt
  , _mkMethod "setSelection" [TInt, TInt] TVoid
  , _mkMethod' "setTextMargins" "setTextMargins" [TObj c_QMargins] TVoid
  , _mkMethod' "setTextMargins" "setTextMarginsRaw" [TInt, TInt, TInt, TInt] TVoid
  , _mkConstMethod "textMargins" [] $ TObj c_QMargins
  , _mkMethod "undo" [] TVoid
  ] ++
  _props
  [ _mkProp "alignment" $ TEnum e_Alignment
    -- TODO completer
  , _mkProp "cursorMoveStyle" $ TEnum e_CursorMoveStyle
  , _mkProp "cursorPosition" TInt
  , _mkProp "dragEnabled" TBool
  , _mkProp "echoMode" $ TEnum e_EchoMode
  , _mkBoolHasProp "frame"
  , _mkProp "inputMask" $ TObj c_QString
  , _mkProp "maxLength" TInt
  , _mkBoolIsProp "modified"
  , _mkProp "placeholderText" $ TObj c_QString
  , _mkBoolIsProp "readOnly"
  , _mkProp "text" $ TObj c_QString
    -- TODO validator
  ]

signals =
  [ _mkSignal "cursorPositionChanged" c_ListenerIntInt
  , _mkSignal "editingFinished" c_Listener
  , _mkSignal "returnPressed" c_Listener
  , _mkSignal "selectionChanged" c_Listener
  , _mkSignal "textEdited" c_ListenerQString
  , _mkSignal "textChanged" c_ListenerQString
  ]

e_EchoMode =
  makeEnum (ident1 "QLineEdit" "EchoMode") Nothing
  [ (0, ["normal"])
  , (1, ["no", "echo"])
  , (2, ["password"])
  , (3, ["password", "echo", "on", "edit"])
  ]
