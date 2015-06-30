module Graphics.UI.Qtah.Internal.Interface.Widgets.QLineEdit (
  cppopModule,
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

{-# ANN module "HLint: ignore Use camelCase" #-}

cppopModule = makeCppopModule "Widgets" "QLineEdit" qtModule

qtModule =
  makeQtModule "Widgets.QLineEdit" $
  QtExport (ExportClass c_QLineEdit) :
  map QtExportSignal signals ++
  [ QtExport $ ExportEnum e_EchoMode ]

this = c_QLineEdit

c_QLineEdit =
  addReqIncludes [includeStd "QLineEdit"] $
  makeClass (ident "QLineEdit") Nothing [c_QWidget]
  [ mkCtor this "new" []
  , mkCtor this "newWithParent" [TPtr $ TObj c_QWidget]
  , mkCtor this "newWithText" [TObj c_QString]
  , mkCtor this "newWithTextAndParent" [TObj c_QString, TPtr $ TObj c_QWidget]
  ] $
  [ mkMethod this "backspace" [] TVoid
  , mkMethod this "clear" [] TVoid
  , mkConstMethod this "copy" [] TVoid
  , mkMethod this "createStandardContextMenu" [] $ TPtr $ TObj c_QMenu
  , mkMethod this "cursorBackward" [TBool, TInt] TVoid
  , mkMethod this "cursorForward" [TBool, TInt] TVoid
  , mkMethod this "cursorPositionAt" [TObj c_QPoint] TInt
  , mkMethod this "cursorWordBackward" [TBool] TVoid
  , mkMethod this "cursorWordForward" [TBool] TVoid
  , mkMethod this "cut" [] TVoid
  , mkMethod this "del" [] TVoid
  , mkMethod this "deselect" [] TVoid
  , mkConstMethod this "displayText" [] $ TObj c_QString
  , mkMethod this "end" [TBool] TVoid
  , mkConstMethod this "hasAcceptableInput" [] TBool
  , mkConstMethod this "hasSelectedText" [] TBool
  , mkMethod this "home" [TBool] TVoid
  , mkMethod this "insert" [TObj c_QString] TVoid
  , mkConstMethod this "isRedoAvailable" [] TBool
  , mkConstMethod this "isUndoAvailable" [] TBool
  , mkMethod this "paste" [] TVoid
  , mkMethod this "redo" [] TVoid
  , mkMethod this "selectAll" [] TVoid
  , mkConstMethod this "selectedText" [] $ TObj c_QString
  , mkConstMethod this "selectionStart" [] TInt
  , mkMethod this "setSelection" [TInt, TInt] TVoid
  , mkMethod' this "setTextMargins" "setTextMargins" [TObj c_QMargins] TVoid
  , mkMethod' this "setTextMargins" "setTextMarginsRaw" [TInt, TInt, TInt, TInt] TVoid
  , mkConstMethod this "textMargins" [] $ TObj c_QMargins
  , mkMethod this "undo" [] TVoid
  ] ++
  mkProps
  [ mkProp this "alignment" $ TEnum e_Alignment
    -- TODO completer
  , mkProp this "cursorMoveStyle" $ TEnum e_CursorMoveStyle
  , mkProp this "cursorPosition" TInt
  , mkProp this "dragEnabled" TBool
  , mkProp this "echoMode" $ TEnum e_EchoMode
  , mkBoolHasProp this "frame"
  , mkProp this "inputMask" $ TObj c_QString
  , mkProp this "maxLength" TInt
  , mkBoolIsProp this "modified"
  , mkProp this "placeholderText" $ TObj c_QString
  , mkBoolIsProp this "readOnly"
  , mkProp this "text" $ TObj c_QString
    -- TODO validator
  ]

signals =
  [ makeSignal this "cursorPositionChanged" c_ListenerIntInt
  , makeSignal this "editingFinished" c_Listener
  , makeSignal this "returnPressed" c_Listener
  , makeSignal this "selectionChanged" c_Listener
  , makeSignal this "textEdited" c_ListenerQString
  , makeSignal this "textChanged" c_ListenerQString
  ]

e_EchoMode =
  makeQtEnum (ident1 "QLineEdit" "EchoMode")
  [ (0, ["normal"])
  , (1, ["no", "echo"])
  , (2, ["password"])
  , (3, ["password", "echo", "on", "edit"])
  ]
