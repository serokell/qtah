module Graphics.UI.Qtah.Internal.Interface.Widgets.QTextEdit (
  cppopModule,
  qtModule,
  ) where

import Foreign.Cppop.Generator.Spec (
  Export (ExportEnum, ExportClass),
  Type (TBitspace, TBool, TEnum, TInt, TObj, TPtr, TVoid),
  addReqIncludes,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkBoolIsProp,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkMethod',
  mkProp,
  mkProps,
  )
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Listener
import Graphics.UI.Qtah.Internal.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Internal.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (bs_Alignment)
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

c_QTextEdit =
  addReqIncludes [includeStd "QTextEdit"] $
  makeClass (ident "QTextEdit") Nothing [c_QAbstractScrollArea]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  , mkCtor "newWithText" [TObj c_QString]
  , mkCtor "newWithTextAndParent" [TObj c_QString, TPtr $ TObj c_QWidget]
  ] $
  [ mkConstMethod "anchorAt" [TObj c_QPoint] $ TObj c_QString
  , mkMethod "append" [TObj c_QString] TVoid
  , mkConstMethod "canPaste" [] TBool
  , mkMethod "clear" [] TVoid
  , mkMethod "copy" [] TVoid
  , mkMethod' "createStandardContextMenu" "createStandardContextMenu" [] $ TPtr $ TObj c_QMenu
  , mkMethod' "createStandardContextMenu" "createStandardContextMenuAt" [TObj c_QPoint] $
    TPtr $ TObj c_QMenu
    -- TODO cursorForPosition
  , mkConstMethod' "cursorRect" "cursorRect" [] $ TObj c_QRect
    -- TODO cursorRect(const QTextCursor&)
  , mkMethod "cut" [] TVoid
  , mkMethod "ensureCursorVisible" [] TVoid
  , mkMethod' "find" "find" [TObj c_QString] TBool
    -- TODO find with FindFlags
  , mkMethod "insertHtml" [TObj c_QString] TVoid
  , mkMethod "insertPlainText" [TObj c_QString] TVoid
    -- TODO loadResource
    -- TODO mergeCurrentCharFormat
    -- TODO moveCursor
  , mkMethod "paste" [] TVoid
    -- TODO print
  , mkMethod "redo" [] TVoid
  , mkMethod "scrollToAnchor" [TObj c_QString] TVoid
  , mkMethod "selectAll" [] TVoid
  , mkMethod "setHtml" [TObj c_QString] TVoid
  , mkMethod "setPlainText" [TObj c_QString] TVoid
  , mkMethod "setText" [TObj c_QString] TVoid
  , mkConstMethod "toHtml" [] $ TObj c_QString
  , mkConstMethod "toPlainText" [] $ TObj c_QString
  , mkMethod "undo" [] TVoid
  , mkMethod "zoomIn" [] TVoid
  , mkMethod' "zoomIn" "zoomInPoints" [TInt] TVoid
  , mkMethod "zoomOut" [] TVoid
  , mkMethod' "zoomOut" "zoomOutPoints" [TInt] TVoid
  ] ++
  mkProps
  [ mkProp "acceptRichText" TBool
  , mkProp "alignment" $ TBitspace bs_Alignment
    -- TODO autoFormatting
    -- TODO currentCharFormat
    -- TODO currentFont
  , mkProp "cursorWidth" TInt
    -- TODO document
  , mkProp "documentTitle" $ TObj c_QString
    -- TODO extraSelections
  , mkProp "fontFamily" $ TObj c_QString
  , mkProp "fontItalic" TBool
    -- TODO fontPointSize
  , mkProp "fontUnderline" TBool
  , mkProp "fontWeight" TInt
  , mkProp "lineWrapColumnOrWidth" TInt
  , mkProp "lineWrapMode" $ TEnum e_LineWrapMode
  , mkProp "overwriteMode" TBool
  , mkBoolIsProp "readOnly"
  , mkProp "tabChangesFocus" TBool
  , mkProp "tabStopWidth" TInt
    -- TODO textBackgroundColor
    -- TODO textColor
    -- TODO textCursor
    -- TODO textInteractionFlags
  , mkBoolIsProp "undoRedoEnabled"
    -- TODO wordWrapMode
  ]

signals =
  [ makeSignal c_QTextEdit "copyAvailable" c_ListenerBool
    -- TODO currentCharFormatChanged
  , makeSignal c_QTextEdit "cursorPositionChanged" c_Listener
  , makeSignal c_QTextEdit "redoAvailable" c_ListenerBool
  , makeSignal c_QTextEdit "selectionChanged" c_Listener
  , makeSignal c_QTextEdit "textChanged" c_Listener
  , makeSignal c_QTextEdit "undoAvailable" c_ListenerBool
  ]

e_LineWrapMode =
  makeQtEnum (ident1 "QTextEdit" "LineWrapMode")
  [ (0, ["no", "wrap"])
  , (1, ["widget", "width"])
  , (2, ["fixed", "pixel", "width"])
  , (3, ["fixed", "column", "width"])
  ]
