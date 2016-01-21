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

module Graphics.UI.Qtah.Internal.Interface.Widgets.QTextEdit (
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
import Graphics.UI.Qtah.Internal.Interface.Listener (
  c_Listener,
  c_ListenerBool,
  )
import Graphics.UI.Qtah.Internal.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Internal.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (bs_Alignment, qreal)
import Graphics.UI.Qtah.Internal.Interface.Gui.QColor (c_QColor)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QAbstractScrollArea (c_QAbstractScrollArea)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QMenu (c_QMenu)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QTextEdit"] $
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
  , mkProp "fontPointSize" qreal
  , mkProp "fontUnderline" TBool
  , mkProp "fontWeight" TInt
  , mkProp "lineWrapColumnOrWidth" TInt
  , mkProp "lineWrapMode" $ TEnum e_LineWrapMode
  , mkProp "overwriteMode" TBool
  , mkBoolIsProp "readOnly"
  , mkProp "tabChangesFocus" TBool
  , mkProp "tabStopWidth" TInt
  , mkProp "textBackgroundColor" $ TObj c_QColor
  , mkProp "textColor" $ TObj c_QColor
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
  makeQtEnum (ident1 "QTextEdit" "LineWrapMode") [includeStd "QTextEdit"]
  [ (0, ["no", "wrap"])
  , (1, ["widget", "width"])
  , (2, ["fixed", "pixel", "width"])
  , (3, ["fixed", "column", "width"])
  ]
