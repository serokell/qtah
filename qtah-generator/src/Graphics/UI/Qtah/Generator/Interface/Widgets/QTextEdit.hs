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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QTextEdit (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportEnum, ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
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
  )
import Foreign.Hoppy.Generator.Types (bitspaceT, boolT, enumT, intT, objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (bs_Alignment, qreal)
import Graphics.UI.Qtah.Generator.Interface.Gui.QColor (c_QColor)
import Graphics.UI.Qtah.Generator.Interface.Gui.QFont (c_QFont)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  c_Listener,
  c_ListenerBool,
  )
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractScrollArea (c_QAbstractScrollArea)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QMenu (c_QMenu)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QTextEdit"] $
  QtExport (ExportClass c_QTextEdit) :
  map QtExportSignal signals ++
  [ QtExport $ ExportEnum e_LineWrapMode ]

c_QTextEdit =
  addReqIncludes [includeStd "QTextEdit"] $
  classSetEntityPrefix "" $
  makeClass (ident "QTextEdit") Nothing [c_QAbstractScrollArea]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , mkCtor "newWithText" [objT c_QString]
  , mkCtor "newWithTextAndParent" [objT c_QString, ptrT $ objT c_QWidget]
  , mkProp "acceptRichText" boolT
  , mkProp "alignment" $ bitspaceT bs_Alignment
  , mkConstMethod "anchorAt" [objT c_QPoint] $ objT c_QString
  , mkMethod "append" [objT c_QString] voidT
    -- TODO autoFormatting
  , mkConstMethod "canPaste" [] boolT
  , mkMethod "clear" [] voidT
  , mkMethod "copy" [] voidT
  , mkMethod' "createStandardContextMenu" "createStandardContextMenu" [] $ ptrT $ objT c_QMenu
  , mkMethod' "createStandardContextMenu" "createStandardContextMenuAt" [objT c_QPoint] $
    ptrT $ objT c_QMenu
    -- TODO currentCharFormat
  , mkProp "currentFont" $ objT c_QFont
    -- TODO cursorForPosition
  , mkConstMethod' "cursorRect" "cursorRect" [] $ objT c_QRect
    -- TODO cursorRect(const QTextCursor&)
  , mkProp "cursorWidth" intT
  , mkMethod "cut" [] voidT
    -- TODO document
  , mkProp "documentTitle" $ objT c_QString
  , mkMethod "ensureCursorVisible" [] voidT
    -- TODO extraSelections
  , mkMethod' "find" "find" [objT c_QString] boolT
    -- TODO find with FindFlags
  , mkProp "fontFamily" $ objT c_QString
  , mkProp "fontItalic" boolT
  , mkProp "fontPointSize" qreal
  , mkProp "fontUnderline" boolT
  , mkProp "fontWeight" intT
  , mkMethod "insertHtml" [objT c_QString] voidT
  , mkMethod "insertPlainText" [objT c_QString] voidT
  , mkProp "lineWrapColumnOrWidth" intT
  , mkProp "lineWrapMode" $ enumT e_LineWrapMode
    -- TODO loadResource
    -- TODO mergeCurrentCharFormat
    -- TODO moveCursor
  , mkProp "overwriteMode" boolT
  , mkMethod "paste" [] voidT
    -- TODO print
  , mkBoolIsProp "readOnly"
  , mkMethod "redo" [] voidT
  , mkMethod "scrollToAnchor" [objT c_QString] voidT
  , mkMethod "selectAll" [] voidT
  , mkMethod "setHtml" [objT c_QString] voidT
  , mkMethod "setPlainText" [objT c_QString] voidT
  , mkMethod "setText" [objT c_QString] voidT
  , mkProp "tabChangesFocus" boolT
  , mkProp "tabStopWidth" intT
  , mkProp "textBackgroundColor" $ objT c_QColor
  , mkProp "textColor" $ objT c_QColor
    -- TODO textCursor
    -- TODO textInteractionFlags
  , mkConstMethod "toHtml" [] $ objT c_QString
  , mkConstMethod "toPlainText" [] $ objT c_QString
  , mkMethod "undo" [] voidT
  , mkBoolIsProp "undoRedoEnabled"
    -- TODO wordWrapMode
  , mkMethod "zoomIn" [] voidT
  , mkMethod' "zoomIn" "zoomInPoints" [intT] voidT
  , mkMethod "zoomOut" [] voidT
  , mkMethod' "zoomOut" "zoomOutPoints" [intT] voidT
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
