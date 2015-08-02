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
  [ just $ mkMethod "aboutQt" [] TVoid
  , just $ mkStaticMethod "activeModalWidget" [] $ TPtr $ TObj c_QWidget
  , just $ mkStaticMethod "activePopupWidget" [] $ TPtr $ TObj c_QWidget
  , just $ mkStaticMethod "alert" [TPtr $ TObj c_QWidget, TInt] TVoid
    -- TODO allWidgets
  , just $ mkStaticMethod "beep" [] TVoid
    -- TODO changeOverrideCursor
    -- TODO clipboard
  , just $ mkMethod "closeAllWindows" [] TVoid
    -- TODO commitData
    -- TODO desktop
  , just $ mkMethod "exec" [] TVoid
  , just $ mkStaticMethod "focusWidget" [] $ TPtr $ TObj c_QWidget
    -- TODO font
    -- TODO fontMetrics
    -- TODO inputContext
    -- TODO isEffectEnabled
  , just $ mkStaticMethod "isLeftToRight" [] TBool
  , just $ mkStaticMethod "isRightToLeft" [] TBool
  , just $ mkConstMethod "isSessionRestored" [] TBool
  , just $ mkStaticMethod "keyboardInputDirection" [] $ TEnum e_LayoutDirection
  , just $ mkStaticMethod "keyboardInputInterval" [] TInt
    -- TODO keyboardInputLocale
    -- TODO keyboardModifiers
  , just $ mkStaticMethod "layoutDirection" [] $ TEnum e_LayoutDirection
    -- TODO macEventFilter
    -- TODO mouseButtons
  , test keypadNavigation $ mkStaticMethod "navigationMode" [] $ TEnum e_NavigationMode
    -- TODO overrideCursor
    -- TODO palette
    -- TODO queryKeyboardModifiers
  , just $ mkStaticMethod "quitOnLastWindowClosed" [] TBool
    -- TODO qwsDecoration
    -- TODO qwsEventFilter
    -- TODO qwsSetCustomColors
    -- TODO qwsSetDecoration
  , just $ mkStaticMethod "restoreOverrideCursor" [] TVoid
    -- TODO saveState
  , just $ mkConstMethod "sessionId" [] $ TObj c_QString
  , just $ mkConstMethod "sessionKey" [] $ TObj c_QString
    -- TODO setEffectEnabled
    -- TODO setFont
    -- TODO setGraphicsSystem
    -- TODO setInputContext
  , just $ mkStaticMethod "setKeyboardInputInterval" [TInt] TVoid
  , just $ mkStaticMethod "setLayoutDirection" [TEnum e_LayoutDirection] TVoid
  , test keypadNavigation $ mkStaticMethod "setNavigationMode" [TEnum e_NavigationMode] TVoid
    -- TODO setOverrideCursor
    -- TODO setPalette
  , just $ mkStaticMethod "setQuitOnLastWindowClosed" [TBool] TVoid
    -- TODO setStyle
    -- TODO style
  , just $ mkStaticMethod "syncX" [] TVoid
    -- TODO symbianEventFilter
    -- TODO symbianProcessEvent
  , just $ mkStaticMethod' "topLevelAt" "topLevelAtPoint" [TObj c_QPoint] $ TPtr $ TObj c_QWidget
  , just $ mkStaticMethod' "topLevelAt" "topLevelAtRaw" [TInt, TInt] $ TPtr $ TObj c_QWidget
    -- TODO topLevelWidgets
    -- We rename type() since @type@ is a Haskell keyword.
  , just $ mkStaticMethod' "type" "applicationType" [] $ TEnum e_Type
  , just $ mkStaticMethod' "widgetAt" "widgetAtPoint" [TObj c_QPoint] $ TPtr $ TObj c_QWidget
  , just $ mkStaticMethod' "widgetAt" "widgetAtRaw" [TInt, TInt] $ TPtr $ TObj c_QWidget
    -- TODO x11EventFilter
    -- TODO x11ProcessEvent
  ] ++
  mkProps
  [ mkStaticProp "activeWindow" $ TPtr $ TObj c_QWidget
  , mkProp "autoSipEnabled" TBool
  , mkStaticProp "colorSpec" TInt
  , mkStaticProp "cursorFlashTime" TInt
  , mkStaticProp "desktopSettingsAware" TBool
  , mkStaticProp "doubleClickInterval" TInt
  , mkStaticProp "globalStrut" $ TObj c_QSize
  , mkProp "startDragDistance" TInt
  , mkProp "startDragTime" TInt
  , mkProp "styleSheet" $ TObj c_QString
  , mkStaticProp "wheelScrollLines" TInt
    -- TODO windowIcon
  ]

signals =
  [ makeSignal c_QApplication "aboutToReleaseGpuResources" c_Listener
  , makeSignal c_QApplication "aboutToUseGpuResources" c_Listener
    -- TODO commitDataRequest
  , makeSignal c_QApplication "focusChanged" c_ListenerPtrQWidgetPtrQWidget
  , makeSignal c_QApplication "fontDatabaseChanged" c_Listener
  , makeSignal c_QApplication "lastWindowClosed" c_Listener
    -- TODO saveStateRequest
  ]

e_Type =
  makeQtEnum (ident1 "QApplication" "Type")
  [ (0, ["tty"])
  , (1, ["gui", "client"])
  , (2, ["gui", "server"])
  ]
