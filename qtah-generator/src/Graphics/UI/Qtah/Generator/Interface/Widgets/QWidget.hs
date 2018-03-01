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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (
  aModule,
  c_QWidget,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkMethod',
  mkProp,
  mkStaticMethod,
  )
import Foreign.Hoppy.Generator.Types (bitspaceT, boolT, enumT, intT, objT, ptrT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (keypadNavigation, qdoc, qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import Graphics.UI.Qtah.Generator.Interface.Core.QMargins (c_QMargins)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (
  bs_WindowFlags,
  bs_WindowStates,
  e_ContextMenuPolicy,
  e_LayoutDirection,
  e_WindowModality,
  e_WindowType,
  qreal,
  )
import Graphics.UI.Qtah.Generator.Interface.Gui.QFont (c_QFont)
import Graphics.UI.Qtah.Generator.Interface.Gui.QIcon (c_QIcon)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPaintDevice (c_QPaintDevice)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPainter (c_QPainter)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPixmap (c_QPixmap)
import Graphics.UI.Qtah.Generator.Interface.Gui.QRegion (c_QRegion)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  c_ListenerQPoint,
  c_ListenerQString,
  c_ListenerRefConstQIcon,
  )
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAction (c_QAction)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QLayout (c_QLayout)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QWidget"] $
  QtExport (ExportClass c_QWidget) :
  map QtExportSignal signals

c_QWidget =
  addReqIncludes [includeStd "QWidget"] $
  classSetEntityPrefix "" $
  makeClass (ident "QWidget") Nothing [c_QObject] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , just $ mkConstMethod "acceptDrops" [] boolT
  , just $ mkConstMethod "accessibleDescription" [] $ objT c_QString
  , just $ mkConstMethod "accessibleName" [] $ objT c_QString
    -- TODO actions
  , just $ mkMethod "activateWindow" [] voidT
  , just $ mkMethod "addAction" [ptrT $ objT c_QAction] voidT
    -- TODO addActions
  , just $ mkMethod "adjustSize" [] voidT
  , just $ mkConstMethod "autoFillBackground" [] boolT
    -- TODO backgroundRole
  , just $ mkConstMethod "baseSize" [] $ objT c_QSize
  , just $ mkConstMethod' "childAt" "childAtRaw" [intT, intT] $ ptrT $ objT c_QWidget
  , just $ mkConstMethod' "childAt" "childAtPoint" [objT c_QPoint] $ ptrT $ objT c_QWidget
  , just $ mkConstMethod "childrenRect" [] $ objT c_QRect
    -- TODO childrenRegion
  , just $ mkMethod "clearFocus" [] voidT
  , just $ mkMethod "clearMask" [] voidT
  , just $ mkMethod "close" [] boolT
  , just $ mkConstMethod "contentsMargins" [] $ objT c_QMargins
  , just $ mkConstMethod "contentsRect" [] $ objT c_QRect
  , just $ mkProp "contextMenuPolicy" $ enumT e_ContextMenuPolicy
    -- TODO cursor
    -- TODO effectiveWinId
  , just $ mkConstMethod "ensurePolished" [] voidT
    -- TODO find
    -- TODO focusPolicy
  , just $ mkConstMethod "focusProxy" [] $ ptrT $ objT c_QWidget
  , just $ mkConstMethod "focusWidget" [] $ ptrT $ objT c_QWidget
  , just $ mkProp "font" $ objT c_QFont
    -- TODO fontInfo
    -- TODO fontMetrics
    -- TODO foregroundRole
  , just $ mkConstMethod "frameGeometry" [] $ objT c_QRect
  , just $ mkConstMethod "frameSize" [] $ objT c_QSize
  , just $ mkConstMethod "geometry" [] $ objT c_QRect
  , test (qtVersion >= [5, 0]) $ mkMethod "grab" [] $ objT c_QPixmap
  , test (qtVersion >= [5, 0]) $
    mkMethod' "grab" "grabWithRect" [objT c_QRect] $ objT c_QPixmap
    -- TODO grabGesture
  , just $ mkMethod "grabKeyboard" [] voidT
  , just $ mkMethod "grabMouse" [] voidT
    -- TODO grabMouse(const QCursor&)
    -- TODO grabShortcut
    -- TODO graphicsEffect
    -- TODO graphicsProxyWidget
  , test keypadNavigation $ mkConstMethod "hasEditFocus" [] boolT
  , just $ mkConstMethod "hasFocus" [] boolT
  , just $ mkConstMethod "hasMouseTracking" [] boolT
  , just $ mkConstMethod "height" [] intT
  , just $ mkConstMethod "heightForWidth" [intT] intT
  , just $ mkMethod "hide" [] voidT
    -- TODO inputContext
    -- TODO inputMethodHints
    -- TODO inputMethodQuery
  , just $ mkMethod "insertAction" [ptrT $ objT c_QAction, ptrT $ objT c_QAction] voidT
    -- TODO insertActions
  , just $ mkConstMethod "isActiveWindow" [] boolT
  , just $ mkConstMethod "isAncestorOf" [ptrT $ objT c_QWidget] boolT
  , just $ mkConstMethod "isEnabled" [] boolT
  , just $ mkConstMethod "isEnabledTo" [ptrT $ objT c_QWidget] boolT
  , just $ mkConstMethod "isFullScreen" [] boolT
  , just $ mkConstMethod "isHidden" [] boolT
  , just $ mkConstMethod "isMaximized" [] boolT
  , just $ mkConstMethod "isMinimized" [] boolT
  , just $ mkConstMethod "isModal" [] boolT
  , just $ mkConstMethod "isVisible" [] boolT
  , just $ mkConstMethod "isVisibleTo" [ptrT $ objT c_QWidget] boolT
  , just $ mkConstMethod "isWindow" [] boolT
  , just $ mkConstMethod "isWindowModified" [] boolT
  , just $ mkStaticMethod "keyboardGrabber" [] $ ptrT $ objT c_QWidget
  , just $ mkConstMethod "layout" [] $ ptrT $ objT c_QLayout
  , just $ mkConstMethod "layoutDirection" [] $ enumT e_LayoutDirection
    -- TODO locale
    -- TODO macCGHandle
    -- TODO macQDHandle
  , just $ mkMethod "lower" [] voidT
  , just $ mkConstMethod "mapFrom" [ptrT $ objT c_QWidget, objT c_QPoint] $ objT c_QPoint
  , just $ mkConstMethod "mapFromGlobal" [objT c_QPoint] $ objT c_QPoint
  , just $ mkConstMethod "mapFromParent" [objT c_QPoint] $ objT c_QPoint
  , just $ mkConstMethod "mapTo" [ptrT $ objT c_QWidget, objT c_QPoint] $ objT c_QPoint
  , just $ mkConstMethod "mapToGlobal" [objT c_QPoint] $ objT c_QPoint
  , just $ mkConstMethod "mapToParent" [objT c_QPoint] $ objT c_QPoint
  , just $ mkConstMethod "maximumHeight" [] intT
  , just $ mkConstMethod "maximumSize" [] $ objT c_QSize
  , just $ mkConstMethod "maximumWidth" [] intT
  , just $ mkConstMethod "minimumHeight" [] intT
  , just $ mkConstMethod "minimumSize" [] $ objT c_QSize
  , just $ mkConstMethod "minimumWidth" [] intT
  , just $ mkStaticMethod "mouseGrabber" [] $ ptrT $ objT c_QWidget
  , just $ mkMethod "move" [objT c_QPoint] voidT
  , just $ mkConstMethod "nativeParentWidget" [] $ ptrT $ objT c_QWidget
  , just $ mkConstMethod "nextInFocusChain" [] $ ptrT $ objT c_QWidget
  , just $ mkConstMethod "normalGeometry" [] $ objT c_QRect
    -- TODO overrideWindowFlags
    -- TODO palette
  , just $ mkConstMethod "parentWidget" [] $ ptrT $ objT c_QWidget
    -- TODO platformWindow
    -- TODO platformWindowFormat
  , just $ mkConstMethod "pos" [] $ objT c_QPoint
  , just $ mkConstMethod "previousInFocusChain" [] $ ptrT $ objT c_QWidget
  , just $ mkMethod "raise" [] voidT
  , just $ mkConstMethod "rect" [] $ objT c_QRect
  , just $ mkMethod "releaseKeyboard" [] voidT
  , just $ mkMethod "releaseMouse" [] voidT
    -- TODO releaseShortcut
  , just $ mkMethod "removeAction" [ptrT $ objT c_QAction] voidT
  , test (qtVersion >= [4, 3]) $
    mkMethod' "render" "renderWithTarget" [ptrT $ objT c_QPaintDevice] voidT
  , test (qtVersion >= [4, 3]) $
    mkMethod' "render" "renderWithTargetAndOffset"
      [ptrT $ objT c_QPaintDevice, objT c_QPoint] voidT
  , test (qtVersion >= [4, 3]) $
    mkMethod' "render" "renderWithTargetAndOffsetAndRegion"
      [ptrT $ objT c_QPaintDevice, objT c_QPoint, objT c_QRegion] voidT
    -- TODO [4.3] void render(QPaintDevice *target, const QPoint &targetOffset,
    --      const QRegion &sourceRegion, RenderFlags renderFlags)
  , just $ mkMethod' "render" "renderWithPainter" [ptrT $ objT c_QPainter] voidT
  , just $
    mkMethod' "render" "renderWithPainterAndOffset"
      [ptrT $ objT c_QPainter, objT c_QPoint] voidT
  , just $
    mkMethod' "render" "renderWithPainterAndOffsetAndRegion"
      [ptrT $ objT c_QPainter, objT c_QPoint, objT c_QRegion] voidT
    -- TODO void render(QPainter *painter, const QPoint &targetOffset, const QRegion &sourceRegion,
    --      RenderFlags renderFlags)
  , just $ mkMethod' "repaint" "repaint" [] voidT
  , just $ mkMethod' "repaint" "repaintRaw" [intT, intT, intT, intT] voidT
  , just $ mkMethod' "repaint" "repaintRect" [objT c_QRect] voidT
    -- TODO repaint(const QRegion&)
  , just $ mkMethod' "resize" "resize" [objT c_QSize] voidT
  , just $ mkMethod' "resize" "resizeRaw" [intT, intT] voidT
  , just $ mkMethod "restoreGeometry" [objT c_QByteArray] boolT
  , just $ mkConstMethod "saveGeometry" [] (objT c_QByteArray)
  , just $ mkMethod' "scroll" "scrollRaw" [intT, intT] voidT
  , just $ mkMethod' "scroll" "scrollRect" [intT, intT, objT c_QRect] voidT
  , just $ mkMethod "setAcceptDrops" [boolT] voidT
  , just $ mkMethod "setAccessibleDescription" [objT c_QString] voidT
  , just $ mkMethod "setAccessibleName" [objT c_QString] voidT
    -- TODO setAttribute
  , just $ mkMethod "setAutoFillBackground" [boolT] voidT
    -- TODO setBackgroundRole
  , just $ mkMethod' "setBaseSize" "setBaseSize" [objT c_QSize] voidT
  , just $ mkMethod' "setBaseSize" "setBaseSizeRaw" [intT, intT] voidT
  , just $ mkMethod' "setContentsMargins" "setContentsMargins" [objT c_QMargins] voidT
  , just $ mkMethod' "setContentsMargins" "setContentsMarginsRaw" [intT, intT, intT, intT] voidT
    -- TODO setContextMenuPolicy
  , just $ mkMethod "setEnabled" [boolT] voidT
  , just $ mkMethod "setDisabled" [boolT] voidT
  , test keypadNavigation $ mkMethod "setEditFocus" [boolT] voidT
  , just $ mkMethod "setFixedHeight" [intT] voidT
  , just $ mkMethod' "setFixedSize" "setFixedSize" [objT c_QSize] voidT
  , just $ mkMethod' "setFixedSize" "setFixedSizeRaw" [intT, intT] voidT
  , just $ mkMethod "setFixedWidth" [intT] voidT
  , just $ mkMethod "setFocus" [] voidT
    -- TODO setFocus(Qt::FocusReason)
    -- TODO setFocusPolicy
  , just $ mkMethod "setFocusProxy" [ptrT $ objT c_QWidget] voidT
    -- TODO setForegroundRole
  , just $ mkMethod' "setGeometry" "setGeometryRaw" [intT, intT, intT, intT] voidT
  , just $ mkMethod' "setGeometry" "setGeometryRect" [objT c_QRect] voidT
    -- TODO setGraphicsEffect
  , just $ mkMethod "setHidden" [boolT] voidT
    -- TODO setInputContext
    -- TODO setInputMethodHints
  , just $ mkMethod "setLayout" [ptrT $ objT c_QLayout] voidT
  , just $ mkMethod "setLayoutDirection" [enumT e_LayoutDirection] voidT
    -- TODO setLocale
    -- TODO setMask
  , just $ mkMethod "setMaximumHeight" [intT] voidT
  , just $ mkMethod' "setMaximumSize" "setMaximumSize" [objT c_QSize] voidT
  , just $ mkMethod' "setMaximumSize" "setMaximumSizeRaw" [intT, intT] voidT
  , just $ mkMethod "setMaximumWidth" [intT] voidT
  , just $ mkMethod "setMinimumHeight" [intT] voidT
  , just $ mkMethod' "setMinimumSize" "setMinimumSize" [objT c_QSize] voidT
  , just $ mkMethod' "setMinimumSize" "setMinimumSizeRaw" [intT, intT] voidT
  , just $ mkMethod "setMinimumWidth" [intT] voidT
  , just $ mkMethod "setMouseTracking" [boolT] voidT
    -- TODO setPalette
  , just $ mkMethod' "setParent" "setParent" [ptrT $ objT c_QWidget] voidT
  , just $ mkMethod' "setParent" "setParentWithFlags"
    [ptrT $ objT c_QWidget, bitspaceT bs_WindowFlags] voidT
    -- TODO setPlatformWindow
    -- TODO setPlatformWindowFormat
    -- TODO setShortcutAutoRepeat
    -- TODO setShortcutEnabled
  , just $ mkMethod' "setSizeIncrement" "setSizeIncrement" [objT c_QSize] voidT
  , just $ mkMethod' "setSizeIncrement" "setSizeIncrementRaw" [intT, intT] voidT
    -- TODO setSizePolicy
  , just $ mkMethod "setStatusTip" [objT c_QString] voidT
    -- TODO setStyle
  , just $ mkMethod "setStyleSheet" [objT c_QString] voidT
  , just $ mkStaticMethod "setTabOrder" [ptrT $ objT c_QWidget, ptrT $ objT c_QWidget] voidT
  , just $ mkMethod "setToolTip" [objT c_QString] voidT
  , just $ mkMethod "setUpdatesEnabled" [boolT] voidT
  , just $ mkMethod "setVisible" [boolT] voidT
  , just $ mkMethod "setWhatsThis" [objT c_QString] voidT
  , just $ mkMethod "setWindowFilePath" [objT c_QString] voidT
  , just $ mkMethod "setWindowIconText" [objT c_QString] voidT
  , just $ mkMethod "setWindowModified" [boolT] voidT
  , just $ mkMethod "setWindowRole" [objT c_QString] voidT
    -- TODO setWindowSurface
  , test qdoc $ mkMethod "setupUi" [ptrT $ objT c_QWidget] voidT
  , just $ mkMethod "show" [] voidT
  , just $ mkMethod "showFullScreen" [] voidT
  , just $ mkMethod "showMaximized" [] voidT
  , just $ mkMethod "showMinimized" [] voidT
  , just $ mkMethod "showNormal" [] voidT
  , just $ mkConstMethod "size" [] $ objT c_QSize
  , just $ mkConstMethod "sizeHint" [] $ objT c_QSize
  , just $ mkConstMethod "sizeIncrement" [] $ objT c_QSize
    -- TODO sizePolicy
  , just $ mkMethod "stackUnder" [ptrT $ objT c_QWidget] voidT
  , just $ mkConstMethod "statusTip" [] $ objT c_QString
  , just $ mkConstMethod "styleSheet" [] $ objT c_QString
    -- TODO testAttribute
  , just $ mkConstMethod "toolTip" [] $ objT c_QString
  , just $ mkConstMethod "underMouse" [] boolT
    -- TODO ungrabGesture
  , just $ mkMethod "unsetCursor" [] voidT
  , just $ mkMethod "unsetLayoutDirection" [] voidT
  , just $ mkMethod "unsetLocale" [] voidT
  , just $ mkMethod' "update" "update" [] voidT
  , just $ mkMethod' "update" "updateRaw" [intT, intT, intT, intT] voidT
  , just $ mkMethod' "update" "updateRect" [objT c_QRect] voidT
    -- TODO update(const QRegion&)
  , just $ mkMethod "updateGeometry" [] voidT
  , just $ mkConstMethod "updatesEnabled" [] boolT
    -- TODO visibleRegion
  , just $ mkConstMethod "whatsThis" [] $ objT c_QString
  , just $ mkConstMethod "width" [] intT
  , just $ mkConstMethod "window" [] $ ptrT $ objT c_QWidget
  , just $ mkConstMethod "windowFilePath" [] $ objT c_QString
  , just $ mkProp "windowFlags" $ bitspaceT bs_WindowFlags
  , just $ mkProp "windowIcon" $ objT c_QIcon
  , -- DEPRECATED by 5.7.
    just $ mkConstMethod "windowIconText" [] $ objT c_QString
  , just $ mkProp "windowModality" $ enumT e_WindowModality
  , just $ mkProp "windowOpacity" qreal
  , just $ mkConstMethod "windowRole" [] $ objT c_QString
    -- TODO windowSurface
  , just $ mkProp "windowState" $ bitspaceT bs_WindowStates
  , just $ mkProp "windowTitle" $ objT c_QString
  , test (qtVersion < [5, 0]) $ mkConstMethod "windowType" [] $ enumT e_WindowType
    -- TODO winId
  , just $ mkConstMethod "x" [] intT
    -- TODO x11Info
    -- TODO x11PictureHandle
  , just $ mkConstMethod "y" [] intT
  ]

signals =
  collect
  [ just $ makeSignal c_QWidget "customContextMenuRequested" c_ListenerQPoint
  , test (qtVersion >= [5, 0]) $ makeSignal c_QWidget "windowIconChanged" c_ListenerRefConstQIcon
    -- TODO windowIconTextChanged (>=5.0?  Deprecated by 5.7.)
  , test (qtVersion >= [5, 0]) $ makeSignal c_QWidget "windowTitleChangd" c_ListenerQString
  ]
