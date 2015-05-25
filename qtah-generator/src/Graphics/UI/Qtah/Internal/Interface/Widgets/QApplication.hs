{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Widgets.QApplication (
  cppopModule,
  qtModule,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Flag (collect, just, test)
import Graphics.UI.Qtah.Internal.Flags (keypadNavigation)
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QCoreApplication (c_QCoreApplication)
import Graphics.UI.Qtah.Internal.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Internal.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (e_LayoutDirection, e_NavigationMode)
import Graphics.UI.Qtah.Internal.Interface.Listener (c_Listener, c_ListenerPtrQWidgetPtrQWidget)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)
#include "../Mk.hs.inc"

cppopModule = makeCppopModule "Widgets" "QApplication" qtModule

qtModule =
  makeQtModule "Widgets.QApplication" $
  [ QtExport $ ExportFn f_QApplication_new
  , QtExport $ ExportClass c_QApplication
  ] ++ map QtExportSignal signals ++
  [ QtExport $ ExportEnum e_Type ]

this = c_QApplication

f_QApplication_new =
  addReqIncludes [includeLocal "shim_qapplication.hpp"] $
  makeFn (ident1 "qtah" "shim_QApplication_new") (Just $ toExtName "QApplication_new") Nonpure
  [] $ TPtr $ TObj c_QApplication

c_QApplication =
  addReqIncludes [includeStd "QApplication"] $
  makeClass (ident "QApplication") Nothing [c_QCoreApplication]
  [ -- TODO Write proper "new" functions for QApplication.
    {-Ctor (toExtName "QApplication_new") []-}
  ] $
  collect
  [ just $ _mkMethod "aboutQt" [] TVoid
  , just $ _mkStaticMethod "activeModalWidget" [] $ TPtr $ TObj c_QWidget
  , just $ _mkStaticMethod "activePopupWidget" [] $ TPtr $ TObj c_QWidget
  , just $ _mkStaticMethod "alert" [TPtr $ TObj c_QWidget, TInt] TVoid
    -- TODO allWidgets
  , just $ _mkStaticMethod "beep" [] TVoid
    -- TODO changeOverrideCursor
    -- TODO clipboard
  , just $ _mkMethod "closeAllWindows" [] TVoid
    -- TODO commitData
    -- TODO desktop
  , just $ _mkMethod "exec" [] TVoid
  , just $ _mkStaticMethod "focusWidget" [] $ TPtr $ TObj c_QWidget
    -- TODO font
    -- TODO fontMetrics
    -- TODO inputContext
    -- TODO isEffectEnabled
  , just $ _mkStaticMethod "isLeftToRight" [] TBool
  , just $ _mkStaticMethod "isRightToLeft" [] TBool
  , just $ _mkConstMethod "isSessionRestored" [] TBool
  , just $ _mkStaticMethod "keyboardInputDirection" [] $ TEnum e_LayoutDirection
  , just $ _mkStaticMethod "keyboardInputInterval" [] TInt
    -- TODO keyboardInputLocale
    -- TODO keyboardModifiers
  , just $ _mkStaticMethod "layoutDirection" [] $ TEnum e_LayoutDirection
    -- TODO macEventFilter
    -- TODO mouseButtons
  , test keypadNavigation $ _mkStaticMethod "navigationMode" [] $ TEnum e_NavigationMode
    -- TODO overrideCursor
    -- TODO palette
    -- TODO queryKeyboardModifiers
  , just $ _mkStaticMethod "quitOnLastWindowClosed" [] TBool
    -- TODO qwsDecoration
    -- TODO qwsEventFilter
    -- TODO qwsSetCustomColors
    -- TODO qwsSetDecoration
  , just $ _mkStaticMethod "restoreOverrideCursor" [] TVoid
    -- TODO saveState
  , just $ _mkConstMethod "sessionId" [] $ TObj c_QString
  , just $ _mkConstMethod "sessionKey" [] $ TObj c_QString
    -- TODO setEffectEnabled
    -- TODO setFont
    -- TODO setGraphicsSystem
    -- TODO setInputContext
  , just $ _mkStaticMethod "setKeyboardInputInterval" [TInt] TVoid
  , just $ _mkStaticMethod "setLayoutDirection" [TEnum e_LayoutDirection] TVoid
  , test keypadNavigation $ _mkStaticMethod "setNavigationMode" [TEnum e_NavigationMode] TVoid
    -- TODO setOverrideCursor
    -- TODO setPalette
  , just $ _mkStaticMethod "setQuitOnLastWindowClosed" [TBool] TVoid
    -- TODO setStyle
    -- TODO style
  , just $ _mkStaticMethod "syncX" [] TVoid
    -- TODO symbianEventFilter
    -- TODO symbianProcessEvent
  , just $ _mkStaticMethod' "topLevelAt" "topLevelAtPoint" [TObj c_QPoint] $ TPtr $ TObj c_QWidget
  , just $ _mkStaticMethod' "topLevelAt" "topLevelAtRaw" [TInt, TInt] $ TPtr $ TObj c_QWidget
    -- TODO topLevelWidgets
    -- We rename type() since @type@ is a Haskell keyword.
  , just $ _mkStaticMethod' "type" "applicationType" [] $ TEnum e_Type
  , just $ _mkStaticMethod' "widgetAt" "widgetAtPoint" [TObj c_QPoint] $ TPtr $ TObj c_QWidget
  , just $ _mkStaticMethod' "widgetAt" "widgetAtRaw" [TInt, TInt] $ TPtr $ TObj c_QWidget
    -- TODO x11EventFilter
    -- TODO x11ProcessEvent
  ] ++
  _props
  [ _mkStaticProp "activeWindow" $ TPtr $ TObj c_QWidget
  , _mkProp "autoSipEnabled" TBool
  , _mkStaticProp "colorSpec" TInt
  , _mkStaticProp "cursorFlashTime" TInt
  , _mkStaticProp "desktopSettingsAware" TBool
  , _mkStaticProp "doubleClickInterval" TInt
  , _mkStaticProp "globalStrut" $ TObj c_QSize
  , _mkProp "startDragDistance" TInt
  , _mkProp "startDragTime" TInt
  , _mkProp "styleSheet" $ TObj c_QString
  , _mkStaticProp "wheelScrollLines" TInt
    -- TODO windowIcon
  ]

signals =
  [ _mkSignal "aboutToReleaseGpuResources" c_Listener
  , _mkSignal "aboutToUseGpuResources" c_Listener
    -- TODO commitDataRequest
  , _mkSignal "focusChanged" c_ListenerPtrQWidgetPtrQWidget
  , _mkSignal "fontDatabaseChanged" c_Listener
  , _mkSignal "lastWindowClosed" c_Listener
    -- TODO saveStateRequest
  ]

e_Type =
  makeEnum (ident1 "QApplication" "Type") Nothing
  [ (0, ["tty"])
  , (1, ["gui", "client"])
  , (2, ["gui", "server"])
  ]
