{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (
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
import Graphics.UI.Qtah.Internal.Interface.Listener (c_ListenerQPoint)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QAction (c_QAction)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QLayout (c_QLayout)
import Graphics.UI.Qtah.Internal.Interface.Qt (e_LayoutDirection)

{-# ANN module "HLint: ignore Use camelCase" #-}

qtModule = makeQtModuleForClass c_QWidget $ map QtExportSignal signals

this = c_QWidget
#include "../Mk.hs.inc"

c_QWidget =
  addReqIncludes [includeStd "QWidget"] $
  makeClass (ident "QWidget") Nothing
  [ c_QObject ]
  [ _mkCtor "new" []
  , _mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  ]
  (collect
   [ just $ _mkConstMethod "acceptDrops" [] TBool
   , just $ _mkConstMethod "accessibleDescription" [] $ TObj c_QString
   , just $ _mkConstMethod "accessibleName" [] $ TObj c_QString
     -- TODO actions
   , just $ _mkMethod "activateWindow" [] TVoid
   , just $ _mkMethod "addAction" [TPtr $ TObj c_QAction] TVoid
     -- TODO addActions
   , just $ _mkMethod "adjustSize" [] TVoid
   , just $ _mkConstMethod "autoFillBackground" [] TBool
     -- TODO backgroundRole
   , just $ _mkConstMethod "baseSize" [] $ TObj c_QSize
   , just $ _mkConstMethod' "childAt" "childAtRaw" [TInt, TInt] $ TPtr $ TObj c_QWidget
   , just $ _mkConstMethod' "childAt" "childAtPoint" [TObj c_QPoint] $ TPtr $ TObj c_QWidget
   , just $ _mkConstMethod "childrenRect" [] $ TObj c_QRect
     -- TODO childrenRegion
   , just $ _mkMethod "clearFocus" [] TVoid
   , just $ _mkMethod "clearMask" [] TVoid
   , just $ _mkMethod "close" [] TVoid
   , just $ _mkConstMethod "contentsMargins" [] $ TObj c_QMargins
   , just $ _mkConstMethod "contentsRect" [] $ TObj c_QRect
     -- TODO contextMenuPolicy
     -- TODO cursor
     -- TODO effectiveWinId
   , just $ _mkConstMethod "ensurePolished" [] TVoid
     -- TODO focusPolicy
   , just $ _mkConstMethod "focusProxy" [] $ TPtr $ TObj c_QWidget
   , just $ _mkConstMethod "focusWidget" [] $ TPtr $ TObj c_QWidget
     -- TODO font
     -- TODO fontInfo
     -- TODO fontMetrics
     -- TODO foregroundRole
   , just $ _mkConstMethod "frameGeometry" [] $ TObj c_QRect
   , just $ _mkConstMethod "frameSize" [] $ TObj c_QSize
   , just $ _mkConstMethod "geometry" [] $ TObj c_QRect
     -- TODO grabGesture
   , just $ _mkMethod "grabKeyboard" [] TVoid
   , just $ _mkMethod "grabMouse" [] TVoid
     -- TODO grabMouse(const QCursor&)
     -- TODO grabShortcut
     -- TODO graphicsEffect
     -- TODO graphicsProxyWidget
   , test keypadNavigation $ _mkConstMethod "hasEditFocus" [] TBool
   , just $ _mkConstMethod "hasFocus" [] TBool
   , just $ _mkConstMethod "hasMouseTracking" [] TBool
   , just $ _mkConstMethod "height" [] TInt
   , just $ _mkConstMethod "heightForWidth" [TInt] TInt
   , just $ _mkMethod "hide" [] TVoid
     -- TODO inputContext
     -- TODO inputMethodHints
     -- TODO inputMethodQuery
   , just $ _mkMethod "insertAction" [TPtr $ TObj c_QAction, TPtr $ TObj c_QAction] TVoid
     -- TODO insertActions
   , just $ _mkConstMethod "isActiveWindow" [] TBool
   , just $ _mkConstMethod "isAncestorOf" [TPtr $ TObj c_QWidget] TBool
   , just $ _mkConstMethod "isEnabled" [] TBool
   , just $ _mkConstMethod "isEnabledTo" [TPtr $ TObj c_QWidget] TBool
   , just $ _mkConstMethod "isFullScreen" [] TBool
   , just $ _mkConstMethod "isHidden" [] TBool
   , just $ _mkConstMethod "isMaximized" [] TBool
   , just $ _mkConstMethod "isMinimized" [] TBool
   , just $ _mkConstMethod "isModal" [] TBool
   , just $ _mkConstMethod "isVisible" [] TBool
   , just $ _mkConstMethod "isVisibleTo" [TPtr $ TObj c_QWidget] TBool
   , just $ _mkConstMethod "isWindow" [] TBool
   , just $ _mkConstMethod "isWindowModified" [] TBool
   , just $ _mkStaticMethod "keyboardGrabber" [] $ TPtr $ TObj c_QWidget
   , just $ _mkConstMethod "layout" [] $ TPtr $ TObj c_QLayout
   , just $ _mkConstMethod "layoutDirection" [] $ TEnum e_LayoutDirection
     -- TODO locale
     -- TODO macCGHandle
     -- TODO macQDHandle
   , just $ _mkMethod "lower" [] TVoid
   , just $ _mkConstMethod "mapFrom" [TPtr $ TObj c_QWidget, TObj c_QPoint] $ TObj c_QPoint
   , just $ _mkConstMethod "mapFromGlobal" [TObj c_QPoint] $ TObj c_QPoint
   , just $ _mkConstMethod "mapFromParent" [TObj c_QPoint] $ TObj c_QPoint
   , just $ _mkConstMethod "mapTo" [TPtr $ TObj c_QWidget, TObj c_QPoint] $ TObj c_QPoint
   , just $ _mkConstMethod "mapToGlobal" [TObj c_QPoint] $ TObj c_QPoint
   , just $ _mkConstMethod "mapToParent" [TObj c_QPoint] $ TObj c_QPoint
   , just $ _mkConstMethod "maximumHeight" [] TInt
   , just $ _mkConstMethod "maximumSize" [] $ TObj c_QSize
   , just $ _mkConstMethod "maximumWidth" [] TInt
   , just $ _mkConstMethod "minimumHeight" [] TInt
   , just $ _mkConstMethod "minimumSize" [] $ TObj c_QSize
   , just $ _mkConstMethod "minimumWidth" [] TInt
   , just $ _mkStaticMethod "mouseGrabber" [] $ TPtr $ TObj c_QWidget
   , just $ _mkMethod "move" [TObj c_QPoint] TVoid
   , just $ _mkConstMethod "nativeParentWidget" [] $ TPtr $ TObj c_QWidget
   , just $ _mkConstMethod "nextInFocusChain" [] $ TPtr $ TObj c_QWidget
   , just $ _mkConstMethod "normalGeometry" [] $ TObj c_QRect
     -- TODO overrideWindowFlags
     -- TODO palette
   , just $ _mkConstMethod "parentWidget" [] $ TPtr $ TObj c_QWidget
     -- TODO platformWindow
     -- TODO platformWindowFormat
   , just $ _mkConstMethod "pos" [] $ TObj c_QPoint
   , just $ _mkConstMethod "previousInFocusChain" [] $ TPtr $ TObj c_QWidget
   , just $ _mkMethod "raise" [] TVoid
   , just $ _mkConstMethod "rect" [] $ TObj c_QRect
   , just $ _mkMethod "releaseKeyboard" [] TVoid
   , just $ _mkMethod "releaseMouse" [] TVoid
     -- TODO releaseShortcut
   , just $ _mkMethod "removeAction" [TPtr $ TObj c_QAction] TVoid
     -- TODO render
   , just $ _mkMethod' "repaint" "repaint" [] TVoid
   , just $ _mkMethod' "repaint" "repaintRaw" [TInt, TInt, TInt, TInt] TVoid
   , just $ _mkMethod' "repaint" "repaintRect" [TObj c_QRect] TVoid
     -- TODO repaint(const QRegion&)
   , just $ _mkMethod' "resize" "resize" [TObj c_QSize] TVoid
   , just $ _mkMethod' "resize" "resizeRaw" [TInt, TInt] TVoid
     -- TODO restoreGeometry
     -- TODO saveGeometry
   , just $ _mkMethod' "scroll" "scrollRaw" [TInt, TInt] TVoid
   , just $ _mkMethod' "scroll" "scrollRect" [TInt, TInt, TObj c_QRect] TVoid
   , just $ _mkMethod "setAcceptDrops" [TBool] TVoid
   , just $ _mkMethod "setAccessibleDescription" [TObj c_QString] TVoid
   , just $ _mkMethod "setAccessibleName" [TObj c_QString] TVoid
     -- TODO setAttribute
   , just $ _mkMethod "setAutoFillBackground" [TBool] TVoid
     -- TODO setBackgroundRole
   , just $ _mkMethod' "setBaseSize" "setBaseSize" [TObj c_QSize] TVoid
   , just $ _mkMethod' "setBaseSize" "setBaseSizeRaw" [TInt, TInt] TVoid
   , just $ _mkMethod' "setContentsMargins" "setContentsMargins" [TObj c_QMargins] TVoid
   , just $ _mkMethod' "setContentsMargins" "setContentsMarginsRaw" [TInt, TInt, TInt, TInt] TVoid
     -- TODO setContextMenuPolicy
   , just $ _mkMethod "setEnabled" [TBool] TVoid
   , just $ _mkMethod "setDisabled" [TBool] TVoid
   , test keypadNavigation $ _mkMethod "setEditFocus" [TBool] TVoid
   , just $ _mkMethod "setFixedHeight" [TInt] TVoid
   , just $ _mkMethod' "setFixedSize" "setFixedSize" [TObj c_QSize] TVoid
   , just $ _mkMethod' "setFixedSize" "setFixedSizeRaw" [TInt, TInt] TVoid
   , just $ _mkMethod "setFixedWidth" [TInt] TVoid
   , just $ _mkMethod "setFocus" [] TVoid
     -- TODO setFocus(Qt::FocusReason)
     -- TODO setFocusPolicy
   , just $ _mkMethod "setFocusProxy" [TPtr $ TObj c_QWidget] TVoid
     -- TODO setFont
     -- TODO setForegroundRole
   , just $ _mkMethod' "setGeometry" "setGeometryRaw" [TInt, TInt, TInt, TInt] TVoid
   , just $ _mkMethod' "setGeometry" "setGeometryRect" [TObj c_QRect] TVoid
     -- TODO setGraphicsEffect
   , just $ _mkMethod "setHidden" [TBool] TVoid
     -- TODO setInputContext
     -- TODO setInputMethodHints
   , just $ _mkMethod "setLayout" [TPtr $ TObj c_QLayout] TVoid
   , just $ _mkMethod "setLayoutDirection" [TEnum e_LayoutDirection] TVoid
     -- TODO setLocale
     -- TODO setMask
   , just $ _mkMethod "setMaximumHeight" [TInt] TVoid
   , just $ _mkMethod' "setMaximumSize" "setMaximumSize" [TObj c_QSize] TVoid
   , just $ _mkMethod' "setMaximumSize" "setMaximumSizeRaw" [TInt, TInt] TVoid
   , just $ _mkMethod "setMaximumWidth" [TInt] TVoid
   , just $ _mkMethod "setMinimumHeight" [TInt] TVoid
   , just $ _mkMethod' "setMinimumSize" "setMinimumSize" [TObj c_QSize] TVoid
   , just $ _mkMethod' "setMinimumSize" "setMinimumSizeRaw" [TInt, TInt] TVoid
   , just $ _mkMethod "setMinimumWidth" [TInt] TVoid
   , just $ _mkMethod "setMouseTracking" [TBool] TVoid
     -- TODO setPalette
   , just $ _mkMethod "setParent" [TPtr $ TObj c_QWidget] TVoid
     -- TODO setParent(QWidget*, Qt::WindowFlags)
     -- TODO setPlatformWindow
     -- TODO setPlatformWindowFormat
     -- TODO setShortcutAutoRepeat
     -- TODO setShortcutEnabled
   , just $ _mkMethod' "setSizeIncrement" "setSizeIncrement" [TObj c_QSize] TVoid
   , just $ _mkMethod' "setSizeIncrement" "setSizeIncrementRaw" [TInt, TInt] TVoid
     -- TODO setSizePolicy
   , just $ _mkMethod "setStatusTip" [TObj c_QString] TVoid
     -- TODO setStyle
   , just $ _mkMethod "setStyleSheet" [TObj c_QString] TVoid
   , just $ _mkStaticMethod "setTabOrder" [TPtr $ TObj c_QWidget, TPtr $ TObj c_QWidget] TVoid
   , just $ _mkMethod "setToolTip" [TObj c_QString] TVoid
   , just $ _mkMethod "setUpdatesEnabled" [TBool] TVoid
   , just $ _mkMethod "setVisible" [TBool] TVoid
   , just $ _mkMethod "setWhatsThis" [TObj c_QString] TVoid
   , just $ _mkMethod "setWindowFilePath" [TObj c_QString] TVoid
     -- TODO setWindowFlags
     -- TODO setWindowIcon
   , just $ _mkMethod "setWindowIconText" [TObj c_QString] TVoid
     -- TODO setWindowModality
   , just $ _mkMethod "setWindowModified" [TBool] TVoid
     -- TODO setWindowOpacity
   , just $ _mkMethod "setWindowRole" [TObj c_QString] TVoid
     -- TODO setWindowState
     -- TODO setWindowSurface
   , just $ _mkMethod "setWindowTitle" [TObj c_QString] TVoid
   , test qdoc $ _mkMethod "setupUi" [TPtr $ TObj c_QWidget] TVoid
   , just $ _mkMethod "show" [] TVoid
   , just $ _mkMethod "showFullScreen" [] TVoid
   , just $ _mkMethod "showMaximized" [] TVoid
   , just $ _mkMethod "showMinimized" [] TVoid
   , just $ _mkMethod "showNormal" [] TVoid
   , just $ _mkConstMethod "size" [] $ TObj c_QSize
   , just $ _mkConstMethod "sizeHint" [] $ TObj c_QSize
   , just $ _mkConstMethod "sizeIncrement" [] $ TObj c_QSize
     -- TODO sizePolicy
   , just $ _mkMethod "stackUnder" [TPtr $ TObj c_QWidget] TVoid
   , just $ _mkConstMethod "statusTip" [] $ TObj c_QString
   , just $ _mkConstMethod "styleSheet" [] $ TObj c_QString
     -- TODO testAttribute
   , just $ _mkConstMethod "toolTip" [] $ TObj c_QString
   , just $ _mkConstMethod "underMouse" [] TBool
     -- TODO ungrabGesture
   , just $ _mkMethod "unsetCursor" [] TVoid
   , just $ _mkMethod "unsetLayoutDirection" [] TVoid
   , just $ _mkMethod "unsetLocale" [] TVoid
   , just $ _mkMethod' "update" "update" [] TVoid
   , just $ _mkMethod' "update" "updateRaw" [TInt, TInt, TInt, TInt] TVoid
   , just $ _mkMethod' "update" "updateRect" [TObj c_QRect] TVoid
     -- TODO update(const QRegion&)
   , just $ _mkMethod "updateGeometry" [] TVoid
   , just $ _mkConstMethod "updatesEnabled" [] TBool
     -- TODO visibleRegion
   , just $ _mkConstMethod "whatsThis" [] $ TObj c_QString
   , just $ _mkConstMethod "width" [] TInt
   , just $ _mkConstMethod "window" [] $ TPtr $ TObj c_QWidget
   , just $ _mkConstMethod "windowFilePath" [] $ TObj c_QString
     -- TODO windowFlags
     -- TODO windowIcon
   , just $ _mkConstMethod "windowIconText" [] $ TObj c_QString
     -- TODO windowModality
     -- TODO windowOpacity
   , just $ _mkConstMethod "windowRole" [] $ TObj c_QString
     -- TODO windowState
     -- TODO windowSurface
   , just $ _mkConstMethod "windowTitle" [] $ TObj c_QString
     -- TODO windowType
     -- TODO winId
   , just $ _mkConstMethod "x" [] TInt
     -- TODO x11Info
     -- TODO x11PictureHandle
   , just $ _mkConstMethod "y" [] TInt
   ])

signals =
  [ _mkSignal "customContextMenuRequested" c_ListenerQPoint
  ]
