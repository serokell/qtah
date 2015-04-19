{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.QTextEdit (
  mod_QTextEdit,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Listener
import Graphics.UI.Qtah.Internal.Interface.QAbstractScrollArea
import Graphics.UI.Qtah.Internal.Interface.QString
import Graphics.UI.Qtah.Internal.Interface.QWidget

this = c_QTextEdit
thisQt = qtc_QTextEdit
#include "MkQt.hs.inc"

mod_QTextEdit =
  makeQtModule "QTextEdit" []
  [ QtExportClass qtc_QTextEdit ]

c_QTextEdit = qtClassClass qtc_QTextEdit

qtc_QTextEdit =
  makeQtClass (ident "QTextEdit") Nothing [c_QAbstractScrollArea]
  [ _mkCtor "new" []
  , _mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  , _mkCtor "newWithText" [TObj c_QString]
  , _mkCtor "newWithTextAndParent" [TObj c_QString, TPtr $ TObj c_QWidget]
  ]
  [ _mkConstMethod "acceptRichText" [] TBool
  , _mkMethod "append" [TObj c_QString] TVoid
  , _mkConstMethod "canPaste" [] TBool
  , _mkMethod "clear" [] TVoid
  , _mkMethod "copy" [] TVoid
  , _mkConstMethod "cursorWidth" [] TInt
  , _mkMethod "cut" [] TVoid
  , _mkConstMethod "documentTitle" [] $ TObj c_QString
  , _mkConstMethod "fontFamily" [] $ TObj c_QString
  , _mkConstMethod "fontItalic" [] TBool
  , _mkConstMethod "fontUnderline" [] TBool
  , _mkConstMethod "fontWeight" [] TInt
  , _mkMethod "insertHtml" [TObj c_QString] TVoid
  , _mkMethod "insertPlainText" [TObj c_QString] TVoid
  , _mkConstMethod "isReadOnly" [] TBool
  , _mkConstMethod "isUndoRedoEnabled" [] TBool
  , _mkConstMethod "lineWrapColumnOrWidth" [] TInt
  , _mkConstMethod "overwriteMode" [] TBool
  , _mkMethod "paste" [] TVoid
  , _mkMethod "redo" [] TVoid
  , _mkMethod "setAcceptRichText" [TBool] TVoid
  , _mkMethod "setCursorWidth" [TInt] TVoid
  , _mkMethod "setDocumentTitle" [TObj c_QString] TVoid
  , _mkMethod "setFontFamily" [TObj c_QString] TVoid
  , _mkMethod "setFontItalic" [TBool] TVoid
  , _mkMethod "setFontUnderline" [TBool] TVoid
  , _mkMethod "setFontWeight" [TInt] TVoid
  , _mkMethod "setHtml" [TObj c_QString] TVoid
  , _mkMethod "setLineWrapColumnOrWidth" [TInt] TVoid
  , _mkMethod "setOverwriteMode" [TBool] TVoid
  , _mkMethod "setPlainText" [TObj c_QString] TVoid
  , _mkMethod "setReadOnly" [TBool] TVoid
  , _mkMethod "setTabChangesFocus" [TBool] TVoid
  , _mkMethod "setTabStopWidth" [TInt] TVoid
  , _mkMethod "setText" [TObj c_QString] TVoid
  , _mkMethod "setUndoRedoEnabled" [TBool] TVoid
  , _mkConstMethod "tabChangesFocus" [] TBool
  , _mkConstMethod "tabStopWidth" [] TInt
  , _mkConstMethod "toHtml" [] $ TObj c_QString
  , _mkConstMethod "toPlainText" [] $ TObj c_QString
  , _mkMethod "undo" [] TVoid
  , _mkMethod "zoomIn" [] TVoid
  , _mkMethod' "zoomIn" "zoomInPoints" [TInt] TVoid
  , _mkMethod "zoomOut" [] TVoid
  , _mkMethod' "zoomOut" "zoomOutPoints" [TInt] TVoid
  ]
  [ _mkSignal "copyAvailable" c_ListenerBool
  , _mkSignal "cursorPositionChanged" c_Listener
  , _mkSignal "redoAvailable" c_ListenerBool
  , _mkSignal "selectionChanged" c_Listener
  , _mkSignal "textChanged" c_Listener
  , _mkSignal "undoAvailable" c_ListenerBool
  ]
