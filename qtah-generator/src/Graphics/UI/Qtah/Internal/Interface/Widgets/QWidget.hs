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

module Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (
  aModule,
  c_QWidget,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  Type (TBool, TEnum, TInt, TObj, TPtr, TVoid),
  addReqIncludes,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkMethod',
  mkStaticMethod,
  )
import Graphics.UI.Qtah.Internal.Flag (collect, just, test)
import Graphics.UI.Qtah.Internal.Flags (keypadNavigation, qdoc)
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QMargins (c_QMargins)
import Graphics.UI.Qtah.Internal.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Internal.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Internal.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Internal.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (
  bs_WindowFlags,
  bs_WindowStates,
  e_LayoutDirection,
  e_WindowModality,
  qreal,
  )
import Graphics.UI.Qtah.Internal.Interface.Listener (c_ListenerQPoint)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QAction (c_QAction)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QLayout (c_QLayout)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QWidget"] $
  QtExport (ExportClass c_QWidget) :
  map QtExportSignal signals

c_QWidget =
  addReqIncludes [includeStd "QWidget"] $
  makeClass (ident "QWidget") Nothing
  [ c_QObject ]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  ] $
  collect
  [ just $ mkConstMethod "acceptDrops" [] TBool
  , just $ mkConstMethod "accessibleDescription" [] $ TObj c_QString
  , just $ mkConstMethod "accessibleName" [] $ TObj c_QString
    -- TODO actions
  , just $ mkMethod "activateWindow" [] TVoid
  , just $ mkMethod "addAction" [TPtr $ TObj c_QAction] TVoid
    -- TODO addActions
  , just $ mkMethod "adjustSize" [] TVoid
  , just $ mkConstMethod "autoFillBackground" [] TBool
    -- TODO backgroundRole
  , just $ mkConstMethod "baseSize" [] $ TObj c_QSize
  , just $ mkConstMethod' "childAt" "childAtRaw" [TInt, TInt] $ TPtr $ TObj c_QWidget
  , just $ mkConstMethod' "childAt" "childAtPoint" [TObj c_QPoint] $ TPtr $ TObj c_QWidget
  , just $ mkConstMethod "childrenRect" [] $ TObj c_QRect
    -- TODO childrenRegion
  , just $ mkMethod "clearFocus" [] TVoid
  , just $ mkMethod "clearMask" [] TVoid
  , just $ mkMethod "close" [] TVoid
  , just $ mkConstMethod "contentsMargins" [] $ TObj c_QMargins
  , just $ mkConstMethod "contentsRect" [] $ TObj c_QRect
    -- TODO contextMenuPolicy
    -- TODO cursor
    -- TODO effectiveWinId
  , just $ mkConstMethod "ensurePolished" [] TVoid
    -- TODO find
    -- TODO focusPolicy
  , just $ mkConstMethod "focusProxy" [] $ TPtr $ TObj c_QWidget
  , just $ mkConstMethod "focusWidget" [] $ TPtr $ TObj c_QWidget
    -- TODO font
    -- TODO fontInfo
    -- TODO fontMetrics
    -- TODO foregroundRole
  , just $ mkConstMethod "frameGeometry" [] $ TObj c_QRect
  , just $ mkConstMethod "frameSize" [] $ TObj c_QSize
  , just $ mkConstMethod "geometry" [] $ TObj c_QRect
    -- TODO grabGesture
  , just $ mkMethod "grabKeyboard" [] TVoid
  , just $ mkMethod "grabMouse" [] TVoid
    -- TODO grabMouse(const QCursor&)
    -- TODO grabShortcut
    -- TODO graphicsEffect
    -- TODO graphicsProxyWidget
  , test keypadNavigation $ mkConstMethod "hasEditFocus" [] TBool
  , just $ mkConstMethod "hasFocus" [] TBool
  , just $ mkConstMethod "hasMouseTracking" [] TBool
  , just $ mkConstMethod "height" [] TInt
  , just $ mkConstMethod "heightForWidth" [TInt] TInt
  , just $ mkMethod "hide" [] TVoid
    -- TODO inputContext
    -- TODO inputMethodHints
    -- TODO inputMethodQuery
  , just $ mkMethod "insertAction" [TPtr $ TObj c_QAction, TPtr $ TObj c_QAction] TVoid
    -- TODO insertActions
  , just $ mkConstMethod "isActiveWindow" [] TBool
  , just $ mkConstMethod "isAncestorOf" [TPtr $ TObj c_QWidget] TBool
  , just $ mkConstMethod "isEnabled" [] TBool
  , just $ mkConstMethod "isEnabledTo" [TPtr $ TObj c_QWidget] TBool
  , just $ mkConstMethod "isFullScreen" [] TBool
  , just $ mkConstMethod "isHidden" [] TBool
  , just $ mkConstMethod "isMaximized" [] TBool
  , just $ mkConstMethod "isMinimized" [] TBool
  , just $ mkConstMethod "isModal" [] TBool
  , just $ mkConstMethod "isVisible" [] TBool
  , just $ mkConstMethod "isVisibleTo" [TPtr $ TObj c_QWidget] TBool
  , just $ mkConstMethod "isWindow" [] TBool
  , just $ mkConstMethod "isWindowModified" [] TBool
  , just $ mkStaticMethod "keyboardGrabber" [] $ TPtr $ TObj c_QWidget
  , just $ mkConstMethod "layout" [] $ TPtr $ TObj c_QLayout
  , just $ mkConstMethod "layoutDirection" [] $ TEnum e_LayoutDirection
    -- TODO locale
    -- TODO macCGHandle
    -- TODO macQDHandle
  , just $ mkMethod "lower" [] TVoid
  , just $ mkConstMethod "mapFrom" [TPtr $ TObj c_QWidget, TObj c_QPoint] $ TObj c_QPoint
  , just $ mkConstMethod "mapFromGlobal" [TObj c_QPoint] $ TObj c_QPoint
  , just $ mkConstMethod "mapFromParent" [TObj c_QPoint] $ TObj c_QPoint
  , just $ mkConstMethod "mapTo" [TPtr $ TObj c_QWidget, TObj c_QPoint] $ TObj c_QPoint
  , just $ mkConstMethod "mapToGlobal" [TObj c_QPoint] $ TObj c_QPoint
  , just $ mkConstMethod "mapToParent" [TObj c_QPoint] $ TObj c_QPoint
  , just $ mkConstMethod "maximumHeight" [] TInt
  , just $ mkConstMethod "maximumSize" [] $ TObj c_QSize
  , just $ mkConstMethod "maximumWidth" [] TInt
  , just $ mkConstMethod "minimumHeight" [] TInt
  , just $ mkConstMethod "minimumSize" [] $ TObj c_QSize
  , just $ mkConstMethod "minimumWidth" [] TInt
  , just $ mkStaticMethod "mouseGrabber" [] $ TPtr $ TObj c_QWidget
  , just $ mkMethod "move" [TObj c_QPoint] TVoid
  , just $ mkConstMethod "nativeParentWidget" [] $ TPtr $ TObj c_QWidget
  , just $ mkConstMethod "nextInFocusChain" [] $ TPtr $ TObj c_QWidget
  , just $ mkConstMethod "normalGeometry" [] $ TObj c_QRect
    -- TODO overrideWindowFlags
    -- TODO palette
  , just $ mkConstMethod "parentWidget" [] $ TPtr $ TObj c_QWidget
    -- TODO platformWindow
    -- TODO platformWindowFormat
  , just $ mkConstMethod "pos" [] $ TObj c_QPoint
  , just $ mkConstMethod "previousInFocusChain" [] $ TPtr $ TObj c_QWidget
  , just $ mkMethod "raise" [] TVoid
  , just $ mkConstMethod "rect" [] $ TObj c_QRect
  , just $ mkMethod "releaseKeyboard" [] TVoid
  , just $ mkMethod "releaseMouse" [] TVoid
    -- TODO releaseShortcut
  , just $ mkMethod "removeAction" [TPtr $ TObj c_QAction] TVoid
    -- TODO render
  , just $ mkMethod' "repaint" "repaint" [] TVoid
  , just $ mkMethod' "repaint" "repaintRaw" [TInt, TInt, TInt, TInt] TVoid
  , just $ mkMethod' "repaint" "repaintRect" [TObj c_QRect] TVoid
    -- TODO repaint(const QRegion&)
  , just $ mkMethod' "resize" "resize" [TObj c_QSize] TVoid
  , just $ mkMethod' "resize" "resizeRaw" [TInt, TInt] TVoid
    -- TODO restoreGeometry
    -- TODO saveGeometry
  , just $ mkMethod' "scroll" "scrollRaw" [TInt, TInt] TVoid
  , just $ mkMethod' "scroll" "scrollRect" [TInt, TInt, TObj c_QRect] TVoid
  , just $ mkMethod "setAcceptDrops" [TBool] TVoid
  , just $ mkMethod "setAccessibleDescription" [TObj c_QString] TVoid
  , just $ mkMethod "setAccessibleName" [TObj c_QString] TVoid
    -- TODO setAttribute
  , just $ mkMethod "setAutoFillBackground" [TBool] TVoid
    -- TODO setBackgroundRole
  , just $ mkMethod' "setBaseSize" "setBaseSize" [TObj c_QSize] TVoid
  , just $ mkMethod' "setBaseSize" "setBaseSizeRaw" [TInt, TInt] TVoid
  , just $ mkMethod' "setContentsMargins" "setContentsMargins" [TObj c_QMargins] TVoid
  , just $ mkMethod' "setContentsMargins" "setContentsMarginsRaw" [TInt, TInt, TInt, TInt] TVoid
    -- TODO setContextMenuPolicy
  , just $ mkMethod "setEnabled" [TBool] TVoid
  , just $ mkMethod "setDisabled" [TBool] TVoid
  , test keypadNavigation $ mkMethod "setEditFocus" [TBool] TVoid
  , just $ mkMethod "setFixedHeight" [TInt] TVoid
  , just $ mkMethod' "setFixedSize" "setFixedSize" [TObj c_QSize] TVoid
  , just $ mkMethod' "setFixedSize" "setFixedSizeRaw" [TInt, TInt] TVoid
  , just $ mkMethod "setFixedWidth" [TInt] TVoid
  , just $ mkMethod "setFocus" [] TVoid
    -- TODO setFocus(Qt::FocusReason)
    -- TODO setFocusPolicy
  , just $ mkMethod "setFocusProxy" [TPtr $ TObj c_QWidget] TVoid
    -- TODO setFont
    -- TODO setForegroundRole
  , just $ mkMethod' "setGeometry" "setGeometryRaw" [TInt, TInt, TInt, TInt] TVoid
  , just $ mkMethod' "setGeometry" "setGeometryRect" [TObj c_QRect] TVoid
    -- TODO setGraphicsEffect
  , just $ mkMethod "setHidden" [TBool] TVoid
    -- TODO setInputContext
    -- TODO setInputMethodHints
  , just $ mkMethod "setLayout" [TPtr $ TObj c_QLayout] TVoid
  , just $ mkMethod "setLayoutDirection" [TEnum e_LayoutDirection] TVoid
    -- TODO setLocale
    -- TODO setMask
  , just $ mkMethod "setMaximumHeight" [TInt] TVoid
  , just $ mkMethod' "setMaximumSize" "setMaximumSize" [TObj c_QSize] TVoid
  , just $ mkMethod' "setMaximumSize" "setMaximumSizeRaw" [TInt, TInt] TVoid
  , just $ mkMethod "setMaximumWidth" [TInt] TVoid
  , just $ mkMethod "setMinimumHeight" [TInt] TVoid
  , just $ mkMethod' "setMinimumSize" "setMinimumSize" [TObj c_QSize] TVoid
  , just $ mkMethod' "setMinimumSize" "setMinimumSizeRaw" [TInt, TInt] TVoid
  , just $ mkMethod "setMinimumWidth" [TInt] TVoid
  , just $ mkMethod "setMouseTracking" [TBool] TVoid
    -- TODO setPalette
  , just $ mkMethod' "setParent" "setParent" [TPtr $ TObj c_QWidget] TVoid
  , just $ mkMethod' "setParent" "setParentWithFlags"
    [TPtr $ TObj c_QWidget, TBitspace bs_WindowFlags] TVoid
    -- TODO setPlatformWindow
    -- TODO setPlatformWindowFormat
    -- TODO setShortcutAutoRepeat
    -- TODO setShortcutEnabled
  , just $ mkMethod' "setSizeIncrement" "setSizeIncrement" [TObj c_QSize] TVoid
  , just $ mkMethod' "setSizeIncrement" "setSizeIncrementRaw" [TInt, TInt] TVoid
    -- TODO setSizePolicy
  , just $ mkMethod "setStatusTip" [TObj c_QString] TVoid
    -- TODO setStyle
  , just $ mkMethod "setStyleSheet" [TObj c_QString] TVoid
  , just $ mkStaticMethod "setTabOrder" [TPtr $ TObj c_QWidget, TPtr $ TObj c_QWidget] TVoid
  , just $ mkMethod "setToolTip" [TObj c_QString] TVoid
  , just $ mkMethod "setUpdatesEnabled" [TBool] TVoid
  , just $ mkMethod "setVisible" [TBool] TVoid
  , just $ mkMethod "setWhatsThis" [TObj c_QString] TVoid
  , just $ mkMethod "setWindowFilePath" [TObj c_QString] TVoid
    -- TODO setWindowIcon
  , just $ mkMethod "setWindowIconText" [TObj c_QString] TVoid
  , just $ mkMethod "setWindowModified" [TBool] TVoid
  , just $ mkMethod "setWindowRole" [TObj c_QString] TVoid
    -- TODO setWindowSurface
  , test qdoc $ mkMethod "setupUi" [TPtr $ TObj c_QWidget] TVoid
  , just $ mkMethod "show" [] TVoid
  , just $ mkMethod "showFullScreen" [] TVoid
  , just $ mkMethod "showMaximized" [] TVoid
  , just $ mkMethod "showMinimized" [] TVoid
  , just $ mkMethod "showNormal" [] TVoid
  , just $ mkConstMethod "size" [] $ TObj c_QSize
  , just $ mkConstMethod "sizeHint" [] $ TObj c_QSize
  , just $ mkConstMethod "sizeIncrement" [] $ TObj c_QSize
    -- TODO sizePolicy
  , just $ mkMethod "stackUnder" [TPtr $ TObj c_QWidget] TVoid
  , just $ mkConstMethod "statusTip" [] $ TObj c_QString
  , just $ mkConstMethod "styleSheet" [] $ TObj c_QString
    -- TODO testAttribute
  , just $ mkConstMethod "toolTip" [] $ TObj c_QString
  , just $ mkConstMethod "underMouse" [] TBool
    -- TODO ungrabGesture
  , just $ mkMethod "unsetCursor" [] TVoid
  , just $ mkMethod "unsetLayoutDirection" [] TVoid
  , just $ mkMethod "unsetLocale" [] TVoid
  , just $ mkMethod' "update" "update" [] TVoid
  , just $ mkMethod' "update" "updateRaw" [TInt, TInt, TInt, TInt] TVoid
  , just $ mkMethod' "update" "updateRect" [TObj c_QRect] TVoid
    -- TODO update(const QRegion&)
  , just $ mkMethod "updateGeometry" [] TVoid
  , just $ mkConstMethod "updatesEnabled" [] TBool
    -- TODO visibleRegion
  , just $ mkConstMethod "whatsThis" [] $ TObj c_QString
  , just $ mkConstMethod "width" [] TInt
  , just $ mkConstMethod "window" [] $ TPtr $ TObj c_QWidget
  , just $ mkConstMethod "windowFilePath" [] $ TObj c_QString
    -- TODO windowIcon
  , just $ mkConstMethod "windowIconText" [] $ TObj c_QString
  , just $ mkConstMethod "windowRole" [] $ TObj c_QString
    -- TODO windowSurface
  , test (qtVersion < [5, 0]) $ mkConstMethod "windowType" [] $ TEnum e_WindowType
    -- TODO winId
  , just $ mkConstMethod "x" [] TInt
    -- TODO x11Info
    -- TODO x11PictureHandle
  , just $ mkConstMethod "y" [] TInt
  ] ++
  mkProps
  [ mkProp "windowFlags" $ TBitspace bs_WindowFlags
  , mkProp "windowModality" $ TEnum e_WindowModality
  , mkProp "windowOpacity" qreal
  , mkProp "windowState" $ TBitspace bs_WindowStates
  , mkProp "windowTitle" $ TObj c_QString
  ]

signals =
  [ makeSignal c_QWidget "customContextMenuRequested" c_ListenerQPoint
    -- TODO windowIconChanged (>=5.0?)
    -- TODO windowIconTextChanged (>=5.0?)
    -- TODO windowTitleChanged (>=5.0?)
  ]
