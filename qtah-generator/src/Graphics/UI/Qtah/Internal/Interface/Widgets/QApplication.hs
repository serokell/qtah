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

{-# ANN module "HLint: ignore Use camelCase" #-}

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
  [ just $ mkMethod this "aboutQt" [] TVoid
  , just $ mkStaticMethod this "activeModalWidget" [] $ TPtr $ TObj c_QWidget
  , just $ mkStaticMethod this "activePopupWidget" [] $ TPtr $ TObj c_QWidget
  , just $ mkStaticMethod this "alert" [TPtr $ TObj c_QWidget, TInt] TVoid
    -- TODO allWidgets
  , just $ mkStaticMethod this "beep" [] TVoid
    -- TODO changeOverrideCursor
    -- TODO clipboard
  , just $ mkMethod this "closeAllWindows" [] TVoid
    -- TODO commitData
    -- TODO desktop
  , just $ mkMethod this "exec" [] TVoid
  , just $ mkStaticMethod this "focusWidget" [] $ TPtr $ TObj c_QWidget
    -- TODO font
    -- TODO fontMetrics
    -- TODO inputContext
    -- TODO isEffectEnabled
  , just $ mkStaticMethod this "isLeftToRight" [] TBool
  , just $ mkStaticMethod this "isRightToLeft" [] TBool
  , just $ mkConstMethod this "isSessionRestored" [] TBool
  , just $ mkStaticMethod this "keyboardInputDirection" [] $ TEnum e_LayoutDirection
  , just $ mkStaticMethod this "keyboardInputInterval" [] TInt
    -- TODO keyboardInputLocale
    -- TODO keyboardModifiers
  , just $ mkStaticMethod this "layoutDirection" [] $ TEnum e_LayoutDirection
    -- TODO macEventFilter
    -- TODO mouseButtons
  , test keypadNavigation $ mkStaticMethod this "navigationMode" [] $ TEnum e_NavigationMode
    -- TODO overrideCursor
    -- TODO palette
    -- TODO queryKeyboardModifiers
  , just $ mkStaticMethod this "quitOnLastWindowClosed" [] TBool
    -- TODO qwsDecoration
    -- TODO qwsEventFilter
    -- TODO qwsSetCustomColors
    -- TODO qwsSetDecoration
  , just $ mkStaticMethod this "restoreOverrideCursor" [] TVoid
    -- TODO saveState
  , just $ mkConstMethod this "sessionId" [] $ TObj c_QString
  , just $ mkConstMethod this "sessionKey" [] $ TObj c_QString
    -- TODO setEffectEnabled
    -- TODO setFont
    -- TODO setGraphicsSystem
    -- TODO setInputContext
  , just $ mkStaticMethod this "setKeyboardInputInterval" [TInt] TVoid
  , just $ mkStaticMethod this "setLayoutDirection" [TEnum e_LayoutDirection] TVoid
  , test keypadNavigation $ mkStaticMethod this "setNavigationMode" [TEnum e_NavigationMode] TVoid
    -- TODO setOverrideCursor
    -- TODO setPalette
  , just $ mkStaticMethod this "setQuitOnLastWindowClosed" [TBool] TVoid
    -- TODO setStyle
    -- TODO style
  , just $ mkStaticMethod this "syncX" [] TVoid
    -- TODO symbianEventFilter
    -- TODO symbianProcessEvent
  , just $ mkStaticMethod' this "topLevelAt" "topLevelAtPoint" [TObj c_QPoint] $ TPtr $ TObj c_QWidget
  , just $ mkStaticMethod' this "topLevelAt" "topLevelAtRaw" [TInt, TInt] $ TPtr $ TObj c_QWidget
    -- TODO topLevelWidgets
    -- We rename type() since @type@ is a Haskell keyword.
  , just $ mkStaticMethod' this "type" "applicationType" [] $ TEnum e_Type
  , just $ mkStaticMethod' this "widgetAt" "widgetAtPoint" [TObj c_QPoint] $ TPtr $ TObj c_QWidget
  , just $ mkStaticMethod' this "widgetAt" "widgetAtRaw" [TInt, TInt] $ TPtr $ TObj c_QWidget
    -- TODO x11EventFilter
    -- TODO x11ProcessEvent
  ] ++
  mkProps
  [ mkStaticProp this "activeWindow" $ TPtr $ TObj c_QWidget
  , mkProp this "autoSipEnabled" TBool
  , mkStaticProp this "colorSpec" TInt
  , mkStaticProp this "cursorFlashTime" TInt
  , mkStaticProp this "desktopSettingsAware" TBool
  , mkStaticProp this "doubleClickInterval" TInt
  , mkStaticProp this "globalStrut" $ TObj c_QSize
  , mkProp this "startDragDistance" TInt
  , mkProp this "startDragTime" TInt
  , mkProp this "styleSheet" $ TObj c_QString
  , mkStaticProp this "wheelScrollLines" TInt
    -- TODO windowIcon
  ]

signals =
  [ makeSignal this "aboutToReleaseGpuResources" c_Listener
  , makeSignal this "aboutToUseGpuResources" c_Listener
    -- TODO commitDataRequest
  , makeSignal this "focusChanged" c_ListenerPtrQWidgetPtrQWidget
  , makeSignal this "fontDatabaseChanged" c_Listener
  , makeSignal this "lastWindowClosed" c_Listener
    -- TODO saveStateRequest
  ]

e_Type =
  makeQtEnum (ident1 "QApplication" "Type")
  [ (0, ["tty"])
  , (1, ["gui", "client"])
  , (2, ["gui", "server"])
  ]
