module Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (
  cppopModule,
  qtModule,
  c_QWidget,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Flag (collect, just, test)
import Graphics.UI.Qtah.Internal.Flags (keypadNavigation, qdoc)
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QMargins (c_QMargins)
import Graphics.UI.Qtah.Internal.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Internal.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Internal.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Internal.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (e_LayoutDirection)
import Graphics.UI.Qtah.Internal.Interface.Listener (c_ListenerQPoint)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QAction (c_QAction)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QLayout (c_QLayout)

{-# ANN module "HLint: ignore Use camelCase" #-}

cppopModule = makeCppopModule "Widgets" "QWidget" qtModule

qtModule =
  makeQtModule "Widgets.QWidget" $
  QtExport (ExportClass c_QWidget) :
  map QtExportSignal signals

this = c_QWidget

c_QWidget =
  addReqIncludes [includeStd "QWidget"] $
  makeClass (ident "QWidget") Nothing
  [ c_QObject ]
  [ mkCtor this "new" []
  , mkCtor this "newWithParent" [TPtr $ TObj c_QWidget]
  ]
  (collect
   [ just $ mkConstMethod this "acceptDrops" [] TBool
   , just $ mkConstMethod this "accessibleDescription" [] $ TObj c_QString
   , just $ mkConstMethod this "accessibleName" [] $ TObj c_QString
     -- TODO actions
   , just $ mkMethod this "activateWindow" [] TVoid
   , just $ mkMethod this "addAction" [TPtr $ TObj c_QAction] TVoid
     -- TODO addActions
   , just $ mkMethod this "adjustSize" [] TVoid
   , just $ mkConstMethod this "autoFillBackground" [] TBool
     -- TODO backgroundRole
   , just $ mkConstMethod this "baseSize" [] $ TObj c_QSize
   , just $ mkConstMethod' this "childAt" "childAtRaw" [TInt, TInt] $ TPtr $ TObj c_QWidget
   , just $ mkConstMethod' this "childAt" "childAtPoint" [TObj c_QPoint] $ TPtr $ TObj c_QWidget
   , just $ mkConstMethod this "childrenRect" [] $ TObj c_QRect
     -- TODO childrenRegion
   , just $ mkMethod this "clearFocus" [] TVoid
   , just $ mkMethod this "clearMask" [] TVoid
   , just $ mkMethod this "close" [] TVoid
   , just $ mkConstMethod this "contentsMargins" [] $ TObj c_QMargins
   , just $ mkConstMethod this "contentsRect" [] $ TObj c_QRect
     -- TODO contextMenuPolicy
     -- TODO cursor
     -- TODO effectiveWinId
   , just $ mkConstMethod this "ensurePolished" [] TVoid
     -- TODO focusPolicy
   , just $ mkConstMethod this "focusProxy" [] $ TPtr $ TObj c_QWidget
   , just $ mkConstMethod this "focusWidget" [] $ TPtr $ TObj c_QWidget
     -- TODO font
     -- TODO fontInfo
     -- TODO fontMetrics
     -- TODO foregroundRole
   , just $ mkConstMethod this "frameGeometry" [] $ TObj c_QRect
   , just $ mkConstMethod this "frameSize" [] $ TObj c_QSize
   , just $ mkConstMethod this "geometry" [] $ TObj c_QRect
     -- TODO grabGesture
   , just $ mkMethod this "grabKeyboard" [] TVoid
   , just $ mkMethod this "grabMouse" [] TVoid
     -- TODO grabMouse(const QCursor&)
     -- TODO grabShortcut
     -- TODO graphicsEffect
     -- TODO graphicsProxyWidget
   , test keypadNavigation $ mkConstMethod this "hasEditFocus" [] TBool
   , just $ mkConstMethod this "hasFocus" [] TBool
   , just $ mkConstMethod this "hasMouseTracking" [] TBool
   , just $ mkConstMethod this "height" [] TInt
   , just $ mkConstMethod this "heightForWidth" [TInt] TInt
   , just $ mkMethod this "hide" [] TVoid
     -- TODO inputContext
     -- TODO inputMethodHints
     -- TODO inputMethodQuery
   , just $ mkMethod this "insertAction" [TPtr $ TObj c_QAction, TPtr $ TObj c_QAction] TVoid
     -- TODO insertActions
   , just $ mkConstMethod this "isActiveWindow" [] TBool
   , just $ mkConstMethod this "isAncestorOf" [TPtr $ TObj c_QWidget] TBool
   , just $ mkConstMethod this "isEnabled" [] TBool
   , just $ mkConstMethod this "isEnabledTo" [TPtr $ TObj c_QWidget] TBool
   , just $ mkConstMethod this "isFullScreen" [] TBool
   , just $ mkConstMethod this "isHidden" [] TBool
   , just $ mkConstMethod this "isMaximized" [] TBool
   , just $ mkConstMethod this "isMinimized" [] TBool
   , just $ mkConstMethod this "isModal" [] TBool
   , just $ mkConstMethod this "isVisible" [] TBool
   , just $ mkConstMethod this "isVisibleTo" [TPtr $ TObj c_QWidget] TBool
   , just $ mkConstMethod this "isWindow" [] TBool
   , just $ mkConstMethod this "isWindowModified" [] TBool
   , just $ mkStaticMethod this "keyboardGrabber" [] $ TPtr $ TObj c_QWidget
   , just $ mkConstMethod this "layout" [] $ TPtr $ TObj c_QLayout
   , just $ mkConstMethod this "layoutDirection" [] $ TEnum e_LayoutDirection
     -- TODO locale
     -- TODO macCGHandle
     -- TODO macQDHandle
   , just $ mkMethod this "lower" [] TVoid
   , just $ mkConstMethod this "mapFrom" [TPtr $ TObj c_QWidget, TObj c_QPoint] $ TObj c_QPoint
   , just $ mkConstMethod this "mapFromGlobal" [TObj c_QPoint] $ TObj c_QPoint
   , just $ mkConstMethod this "mapFromParent" [TObj c_QPoint] $ TObj c_QPoint
   , just $ mkConstMethod this "mapTo" [TPtr $ TObj c_QWidget, TObj c_QPoint] $ TObj c_QPoint
   , just $ mkConstMethod this "mapToGlobal" [TObj c_QPoint] $ TObj c_QPoint
   , just $ mkConstMethod this "mapToParent" [TObj c_QPoint] $ TObj c_QPoint
   , just $ mkConstMethod this "maximumHeight" [] TInt
   , just $ mkConstMethod this "maximumSize" [] $ TObj c_QSize
   , just $ mkConstMethod this "maximumWidth" [] TInt
   , just $ mkConstMethod this "minimumHeight" [] TInt
   , just $ mkConstMethod this "minimumSize" [] $ TObj c_QSize
   , just $ mkConstMethod this "minimumWidth" [] TInt
   , just $ mkStaticMethod this "mouseGrabber" [] $ TPtr $ TObj c_QWidget
   , just $ mkMethod this "move" [TObj c_QPoint] TVoid
   , just $ mkConstMethod this "nativeParentWidget" [] $ TPtr $ TObj c_QWidget
   , just $ mkConstMethod this "nextInFocusChain" [] $ TPtr $ TObj c_QWidget
   , just $ mkConstMethod this "normalGeometry" [] $ TObj c_QRect
     -- TODO overrideWindowFlags
     -- TODO palette
   , just $ mkConstMethod this "parentWidget" [] $ TPtr $ TObj c_QWidget
     -- TODO platformWindow
     -- TODO platformWindowFormat
   , just $ mkConstMethod this "pos" [] $ TObj c_QPoint
   , just $ mkConstMethod this "previousInFocusChain" [] $ TPtr $ TObj c_QWidget
   , just $ mkMethod this "raise" [] TVoid
   , just $ mkConstMethod this "rect" [] $ TObj c_QRect
   , just $ mkMethod this "releaseKeyboard" [] TVoid
   , just $ mkMethod this "releaseMouse" [] TVoid
     -- TODO releaseShortcut
   , just $ mkMethod this "removeAction" [TPtr $ TObj c_QAction] TVoid
     -- TODO render
   , just $ mkMethod' this "repaint" "repaint" [] TVoid
   , just $ mkMethod' this "repaint" "repaintRaw" [TInt, TInt, TInt, TInt] TVoid
   , just $ mkMethod' this "repaint" "repaintRect" [TObj c_QRect] TVoid
     -- TODO repaint(const QRegion&)
   , just $ mkMethod' this "resize" "resize" [TObj c_QSize] TVoid
   , just $ mkMethod' this "resize" "resizeRaw" [TInt, TInt] TVoid
     -- TODO restoreGeometry
     -- TODO saveGeometry
   , just $ mkMethod' this "scroll" "scrollRaw" [TInt, TInt] TVoid
   , just $ mkMethod' this "scroll" "scrollRect" [TInt, TInt, TObj c_QRect] TVoid
   , just $ mkMethod this "setAcceptDrops" [TBool] TVoid
   , just $ mkMethod this "setAccessibleDescription" [TObj c_QString] TVoid
   , just $ mkMethod this "setAccessibleName" [TObj c_QString] TVoid
     -- TODO setAttribute
   , just $ mkMethod this "setAutoFillBackground" [TBool] TVoid
     -- TODO setBackgroundRole
   , just $ mkMethod' this "setBaseSize" "setBaseSize" [TObj c_QSize] TVoid
   , just $ mkMethod' this "setBaseSize" "setBaseSizeRaw" [TInt, TInt] TVoid
   , just $ mkMethod' this "setContentsMargins" "setContentsMargins" [TObj c_QMargins] TVoid
   , just $ mkMethod' this "setContentsMargins" "setContentsMarginsRaw" [TInt, TInt, TInt, TInt] TVoid
     -- TODO setContextMenuPolicy
   , just $ mkMethod this "setEnabled" [TBool] TVoid
   , just $ mkMethod this "setDisabled" [TBool] TVoid
   , test keypadNavigation $ mkMethod this "setEditFocus" [TBool] TVoid
   , just $ mkMethod this "setFixedHeight" [TInt] TVoid
   , just $ mkMethod' this "setFixedSize" "setFixedSize" [TObj c_QSize] TVoid
   , just $ mkMethod' this "setFixedSize" "setFixedSizeRaw" [TInt, TInt] TVoid
   , just $ mkMethod this "setFixedWidth" [TInt] TVoid
   , just $ mkMethod this "setFocus" [] TVoid
     -- TODO setFocus(Qt::FocusReason)
     -- TODO setFocusPolicy
   , just $ mkMethod this "setFocusProxy" [TPtr $ TObj c_QWidget] TVoid
     -- TODO setFont
     -- TODO setForegroundRole
   , just $ mkMethod' this "setGeometry" "setGeometryRaw" [TInt, TInt, TInt, TInt] TVoid
   , just $ mkMethod' this "setGeometry" "setGeometryRect" [TObj c_QRect] TVoid
     -- TODO setGraphicsEffect
   , just $ mkMethod this "setHidden" [TBool] TVoid
     -- TODO setInputContext
     -- TODO setInputMethodHints
   , just $ mkMethod this "setLayout" [TPtr $ TObj c_QLayout] TVoid
   , just $ mkMethod this "setLayoutDirection" [TEnum e_LayoutDirection] TVoid
     -- TODO setLocale
     -- TODO setMask
   , just $ mkMethod this "setMaximumHeight" [TInt] TVoid
   , just $ mkMethod' this "setMaximumSize" "setMaximumSize" [TObj c_QSize] TVoid
   , just $ mkMethod' this "setMaximumSize" "setMaximumSizeRaw" [TInt, TInt] TVoid
   , just $ mkMethod this "setMaximumWidth" [TInt] TVoid
   , just $ mkMethod this "setMinimumHeight" [TInt] TVoid
   , just $ mkMethod' this "setMinimumSize" "setMinimumSize" [TObj c_QSize] TVoid
   , just $ mkMethod' this "setMinimumSize" "setMinimumSizeRaw" [TInt, TInt] TVoid
   , just $ mkMethod this "setMinimumWidth" [TInt] TVoid
   , just $ mkMethod this "setMouseTracking" [TBool] TVoid
     -- TODO setPalette
   , just $ mkMethod this "setParent" [TPtr $ TObj c_QWidget] TVoid
     -- TODO setParent(QWidget*, Qt::WindowFlags)
     -- TODO setPlatformWindow
     -- TODO setPlatformWindowFormat
     -- TODO setShortcutAutoRepeat
     -- TODO setShortcutEnabled
   , just $ mkMethod' this "setSizeIncrement" "setSizeIncrement" [TObj c_QSize] TVoid
   , just $ mkMethod' this "setSizeIncrement" "setSizeIncrementRaw" [TInt, TInt] TVoid
     -- TODO setSizePolicy
   , just $ mkMethod this "setStatusTip" [TObj c_QString] TVoid
     -- TODO setStyle
   , just $ mkMethod this "setStyleSheet" [TObj c_QString] TVoid
   , just $ mkStaticMethod this "setTabOrder" [TPtr $ TObj c_QWidget, TPtr $ TObj c_QWidget] TVoid
   , just $ mkMethod this "setToolTip" [TObj c_QString] TVoid
   , just $ mkMethod this "setUpdatesEnabled" [TBool] TVoid
   , just $ mkMethod this "setVisible" [TBool] TVoid
   , just $ mkMethod this "setWhatsThis" [TObj c_QString] TVoid
   , just $ mkMethod this "setWindowFilePath" [TObj c_QString] TVoid
     -- TODO setWindowFlags
     -- TODO setWindowIcon
   , just $ mkMethod this "setWindowIconText" [TObj c_QString] TVoid
     -- TODO setWindowModality
   , just $ mkMethod this "setWindowModified" [TBool] TVoid
     -- TODO setWindowOpacity
   , just $ mkMethod this "setWindowRole" [TObj c_QString] TVoid
     -- TODO setWindowState
     -- TODO setWindowSurface
   , just $ mkMethod this "setWindowTitle" [TObj c_QString] TVoid
   , test qdoc $ mkMethod this "setupUi" [TPtr $ TObj c_QWidget] TVoid
   , just $ mkMethod this "show" [] TVoid
   , just $ mkMethod this "showFullScreen" [] TVoid
   , just $ mkMethod this "showMaximized" [] TVoid
   , just $ mkMethod this "showMinimized" [] TVoid
   , just $ mkMethod this "showNormal" [] TVoid
   , just $ mkConstMethod this "size" [] $ TObj c_QSize
   , just $ mkConstMethod this "sizeHint" [] $ TObj c_QSize
   , just $ mkConstMethod this "sizeIncrement" [] $ TObj c_QSize
     -- TODO sizePolicy
   , just $ mkMethod this "stackUnder" [TPtr $ TObj c_QWidget] TVoid
   , just $ mkConstMethod this "statusTip" [] $ TObj c_QString
   , just $ mkConstMethod this "styleSheet" [] $ TObj c_QString
     -- TODO testAttribute
   , just $ mkConstMethod this "toolTip" [] $ TObj c_QString
   , just $ mkConstMethod this "underMouse" [] TBool
     -- TODO ungrabGesture
   , just $ mkMethod this "unsetCursor" [] TVoid
   , just $ mkMethod this "unsetLayoutDirection" [] TVoid
   , just $ mkMethod this "unsetLocale" [] TVoid
   , just $ mkMethod' this "update" "update" [] TVoid
   , just $ mkMethod' this "update" "updateRaw" [TInt, TInt, TInt, TInt] TVoid
   , just $ mkMethod' this "update" "updateRect" [TObj c_QRect] TVoid
     -- TODO update(const QRegion&)
   , just $ mkMethod this "updateGeometry" [] TVoid
   , just $ mkConstMethod this "updatesEnabled" [] TBool
     -- TODO visibleRegion
   , just $ mkConstMethod this "whatsThis" [] $ TObj c_QString
   , just $ mkConstMethod this "width" [] TInt
   , just $ mkConstMethod this "window" [] $ TPtr $ TObj c_QWidget
   , just $ mkConstMethod this "windowFilePath" [] $ TObj c_QString
     -- TODO windowFlags
     -- TODO windowIcon
   , just $ mkConstMethod this "windowIconText" [] $ TObj c_QString
     -- TODO windowModality
     -- TODO windowOpacity
   , just $ mkConstMethod this "windowRole" [] $ TObj c_QString
     -- TODO windowState
     -- TODO windowSurface
   , just $ mkConstMethod this "windowTitle" [] $ TObj c_QString
     -- TODO windowType
     -- TODO winId
   , just $ mkConstMethod this "x" [] TInt
     -- TODO x11Info
     -- TODO x11PictureHandle
   , just $ mkConstMethod this "y" [] TInt
   ])

signals =
  [ makeSignal this "customContextMenuRequested" c_ListenerQPoint
  ]
