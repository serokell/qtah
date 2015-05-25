{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Widgets.QTextEdit (
  cppopModule,
  qtModule,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Listener
import Graphics.UI.Qtah.Internal.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Internal.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (e_Alignment)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QAbstractScrollArea (c_QAbstractScrollArea)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QMenu (c_QMenu)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)
#include "../Mk.hs.inc"

cppopModule = makeCppopModule "Widgets" "QTextEdit" qtModule

qtModule =
  makeQtModule "Widgets.QTextEdit" $
  QtExport (ExportClass c_QTextEdit) :
  map QtExportSignal signals ++
  [ QtExport $ ExportEnum e_LineWrapMode ]

this = c_QTextEdit

c_QTextEdit =
  addReqIncludes [includeStd "QTextEdit"] $
  makeClass (ident "QTextEdit") Nothing [c_QAbstractScrollArea]
  [ _mkCtor "new" []
  , _mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  , _mkCtor "newWithText" [TObj c_QString]
  , _mkCtor "newWithTextAndParent" [TObj c_QString, TPtr $ TObj c_QWidget]
  ] $
  [ _mkConstMethod "anchorAt" [TObj c_QPoint] $ TObj c_QString
  , _mkMethod "append" [TObj c_QString] TVoid
  , _mkConstMethod "canPaste" [] TBool
  , _mkMethod "clear" [] TVoid
  , _mkMethod "copy" [] TVoid
  , _mkMethod' "createStandardContextMenu" "createStandardContextMenu" [] $ TPtr $ TObj c_QMenu
  , _mkMethod' "createStandardContextMenu" "createStandardContextMenuAt" [TObj c_QPoint] $
    TPtr $ TObj c_QMenu
    -- TODO cursorForPosition
  , _mkConstMethod' "cursorRect" "cursorRect" [] $ TObj c_QRect
    -- TODO cursorRect(const QTextCursor&)
  , _mkMethod "cut" [] TVoid
  , _mkMethod "ensureCursorVisible" [] TVoid
  , _mkMethod' "find" "find" [TObj c_QString] TBool
    -- TODO find with FindFlags
  , _mkMethod "insertHtml" [TObj c_QString] TVoid
  , _mkMethod "insertPlainText" [TObj c_QString] TVoid
    -- TODO loadResource
    -- TODO mergeCurrentCharFormat
    -- TODO moveCursor
  , _mkMethod "paste" [] TVoid
    -- TODO print
  , _mkMethod "redo" [] TVoid
  , _mkMethod "scrollToAnchor" [TObj c_QString] TVoid
  , _mkMethod "selectAll" [] TVoid
  , _mkMethod "setHtml" [TObj c_QString] TVoid
  , _mkMethod "setPlainText" [TObj c_QString] TVoid
  , _mkMethod "setText" [TObj c_QString] TVoid
  , _mkConstMethod "toHtml" [] $ TObj c_QString
  , _mkConstMethod "toPlainText" [] $ TObj c_QString
  , _mkMethod "undo" [] TVoid
  , _mkMethod "zoomIn" [] TVoid
  , _mkMethod' "zoomIn" "zoomInPoints" [TInt] TVoid
  , _mkMethod "zoomOut" [] TVoid
  , _mkMethod' "zoomOut" "zoomOutPoints" [TInt] TVoid
  ] ++
  _props
  [ _mkProp "acceptRichText" TBool
  , _mkProp "alignment" $ TEnum e_Alignment
    -- TODO autoFormatting
    -- TODO currentCharFormat
    -- TODO currentFont
  , _mkProp "cursorWidth" TInt
    -- TODO document
  , _mkProp "documentTitle" $ TObj c_QString
    -- TODO extraSelections
  , _mkProp "fontFamily" $ TObj c_QString
  , _mkProp "fontItalic" TBool
    -- TODO fontPointSize
  , _mkProp "fontUnderline" TBool
  , _mkProp "fontWeight" TInt
  , _mkProp "lineWrapColumnOrWidth" TInt
  , _mkProp "lineWrapMode" $ TEnum e_LineWrapMode
  , _mkProp "overwriteMode" TBool
  , _mkBoolIsProp "readOnly"
  , _mkProp "tabChangesFocus" TBool
  , _mkProp "tabStopWidth" TInt
    -- TODO textBackgroundColor
    -- TODO textColor
    -- TODO textCursor
    -- TODO textInteractionFlags
  , _mkBoolIsProp "undoRedoEnabled"
    -- TODO wordWrapMode
  ]

signals =
  [ _mkSignal "copyAvailable" c_ListenerBool
    -- TODO currentCharFormatChanged
  , _mkSignal "cursorPositionChanged" c_Listener
  , _mkSignal "redoAvailable" c_ListenerBool
  , _mkSignal "selectionChanged" c_Listener
  , _mkSignal "textChanged" c_Listener
  , _mkSignal "undoAvailable" c_ListenerBool
  ]

e_LineWrapMode =
  makeEnum (ident1 "QTextEdit" "LineWrapMode") Nothing
  [ (0, ["no", "wrap"])
  , (1, ["widget", "width"])
  , (2, ["fixed", "pixel", "width"])
  , (3, ["fixed", "column", "width"])
  ]
