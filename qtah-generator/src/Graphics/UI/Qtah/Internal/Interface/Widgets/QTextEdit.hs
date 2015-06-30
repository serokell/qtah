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

{-# ANN module "HLint: ignore Use camelCase" #-}

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
  [ mkCtor this "new" []
  , mkCtor this "newWithParent" [TPtr $ TObj c_QWidget]
  , mkCtor this "newWithText" [TObj c_QString]
  , mkCtor this "newWithTextAndParent" [TObj c_QString, TPtr $ TObj c_QWidget]
  ] $
  [ mkConstMethod this "anchorAt" [TObj c_QPoint] $ TObj c_QString
  , mkMethod this "append" [TObj c_QString] TVoid
  , mkConstMethod this "canPaste" [] TBool
  , mkMethod this "clear" [] TVoid
  , mkMethod this "copy" [] TVoid
  , mkMethod' this "createStandardContextMenu" "createStandardContextMenu" [] $ TPtr $ TObj c_QMenu
  , mkMethod' this "createStandardContextMenu" "createStandardContextMenuAt" [TObj c_QPoint] $
    TPtr $ TObj c_QMenu
    -- TODO cursorForPosition
  , mkConstMethod' this "cursorRect" "cursorRect" [] $ TObj c_QRect
    -- TODO cursorRect(const QTextCursor&)
  , mkMethod this "cut" [] TVoid
  , mkMethod this "ensureCursorVisible" [] TVoid
  , mkMethod' this "find" "find" [TObj c_QString] TBool
    -- TODO find with FindFlags
  , mkMethod this "insertHtml" [TObj c_QString] TVoid
  , mkMethod this "insertPlainText" [TObj c_QString] TVoid
    -- TODO loadResource
    -- TODO mergeCurrentCharFormat
    -- TODO moveCursor
  , mkMethod this "paste" [] TVoid
    -- TODO print
  , mkMethod this "redo" [] TVoid
  , mkMethod this "scrollToAnchor" [TObj c_QString] TVoid
  , mkMethod this "selectAll" [] TVoid
  , mkMethod this "setHtml" [TObj c_QString] TVoid
  , mkMethod this "setPlainText" [TObj c_QString] TVoid
  , mkMethod this "setText" [TObj c_QString] TVoid
  , mkConstMethod this "toHtml" [] $ TObj c_QString
  , mkConstMethod this "toPlainText" [] $ TObj c_QString
  , mkMethod this "undo" [] TVoid
  , mkMethod this "zoomIn" [] TVoid
  , mkMethod' this "zoomIn" "zoomInPoints" [TInt] TVoid
  , mkMethod this "zoomOut" [] TVoid
  , mkMethod' this "zoomOut" "zoomOutPoints" [TInt] TVoid
  ] ++
  mkProps
  [ mkProp this "acceptRichText" TBool
  , mkProp this "alignment" $ TEnum e_Alignment
    -- TODO autoFormatting
    -- TODO currentCharFormat
    -- TODO currentFont
  , mkProp this "cursorWidth" TInt
    -- TODO document
  , mkProp this "documentTitle" $ TObj c_QString
    -- TODO extraSelections
  , mkProp this "fontFamily" $ TObj c_QString
  , mkProp this "fontItalic" TBool
    -- TODO fontPointSize
  , mkProp this "fontUnderline" TBool
  , mkProp this "fontWeight" TInt
  , mkProp this "lineWrapColumnOrWidth" TInt
  , mkProp this "lineWrapMode" $ TEnum e_LineWrapMode
  , mkProp this "overwriteMode" TBool
  , mkBoolIsProp this "readOnly"
  , mkProp this "tabChangesFocus" TBool
  , mkProp this "tabStopWidth" TInt
    -- TODO textBackgroundColor
    -- TODO textColor
    -- TODO textCursor
    -- TODO textInteractionFlags
  , mkBoolIsProp this "undoRedoEnabled"
    -- TODO wordWrapMode
  ]

signals =
  [ makeSignal this "copyAvailable" c_ListenerBool
    -- TODO currentCharFormatChanged
  , makeSignal this "cursorPositionChanged" c_Listener
  , makeSignal this "redoAvailable" c_ListenerBool
  , makeSignal this "selectionChanged" c_Listener
  , makeSignal this "textChanged" c_Listener
  , makeSignal this "undoAvailable" c_ListenerBool
  ]

e_LineWrapMode =
  makeQtEnum (ident1 "QTextEdit" "LineWrapMode")
  [ (0, ["no", "wrap"])
  , (1, ["widget", "width"])
  , (2, ["fixed", "pixel", "width"])
  , (3, ["fixed", "column", "width"])
  ]
