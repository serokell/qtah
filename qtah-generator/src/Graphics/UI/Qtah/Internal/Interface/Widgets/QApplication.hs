-- This file is part of Qtah.
--
-- Copyright 2015 Bryan Gardiner <bog@khumba.net>
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

module Graphics.UI.Qtah.Internal.Interface.Widgets.QApplication (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportEnum, ExportClass),
  MethodApplicability (MStatic),
  Purity (Nonpure),
  Type (TBool, TEnum, TInt, TObj, TPtr, TVoid),
  addReqIncludes,
  ident,
  ident1,
  ident2,
  includeLocal,
  includeStd,
  makeFnMethod,
  makeClass,
  mkConstMethod,
  mkMethod,
  mkProp,
  mkProps,
  mkStaticMethod,
  mkStaticMethod',
  mkStaticProp,
  )
import Graphics.UI.Qtah.Internal.Flag (collect, just, test)
import Graphics.UI.Qtah.Internal.Flags (keypadNavigation, qtVersion)
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QCoreApplication (c_QCoreApplication)
import Graphics.UI.Qtah.Internal.Interface.Core.QList (c_QListQWidget)
import Graphics.UI.Qtah.Internal.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Internal.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Core.QStringList (c_QStringList)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (e_LayoutDirection, e_NavigationMode)
import Graphics.UI.Qtah.Internal.Interface.Gui.QClipboard (c_QClipboard)
import Graphics.UI.Qtah.Internal.Interface.Listener (c_Listener, c_ListenerPtrQWidgetPtrQWidget)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QApplication"] $
  [ QtExport $ ExportClass c_QApplication
  ] ++ map QtExportSignal signals ++
  collect
  [ test (qtVersion < [5]) $ QtExport $ ExportEnum e_Type
  ]

c_QApplication =
  addReqIncludes [ includeStd "QApplication"
                 , includeLocal "wrap_qapplication.hpp"
                 ] $
  makeClass (ident "QApplication") Nothing [c_QCoreApplication] [] $
  collect
  [ just $ makeFnMethod (ident2 "qtah" "qapplication" "create") "new" MStatic Nonpure
    [TObj c_QStringList] $ TPtr $ TObj c_QApplication
  , just $ mkMethod "aboutQt" [] TVoid
  , just $ mkStaticMethod "activeModalWidget" [] $ TPtr $ TObj c_QWidget
  , just $ mkStaticMethod "activePopupWidget" [] $ TPtr $ TObj c_QWidget
  , just $ mkStaticMethod "alert" [TPtr $ TObj c_QWidget, TInt] TVoid
  , just $ mkStaticMethod "allWidgets" [] $ TObj c_QListQWidget
  , just $ mkStaticMethod "beep" [] TVoid
    -- TODO changeOverrideCursor
  , just $ mkStaticMethod "clipboard" [] $ TPtr $ TObj c_QClipboard
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
  , test (qtVersion < [5]) $ mkStaticMethod "keyboardInputDirection" [] $ TEnum e_LayoutDirection
  , just $ mkStaticMethod "keyboardInputInterval" [] TInt
    -- TODO keyboardInputLocale (<5)
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
    -- TODO setGraphicsSystem (<5)
    -- TODO setInputContext
  , just $ mkStaticMethod "setKeyboardInputInterval" [TInt] TVoid
  , just $ mkStaticMethod "setLayoutDirection" [TEnum e_LayoutDirection] TVoid
  , test keypadNavigation $ mkStaticMethod "setNavigationMode" [TEnum e_NavigationMode] TVoid
    -- TODO setOverrideCursor
    -- TODO setPalette
  , just $ mkStaticMethod "setQuitOnLastWindowClosed" [TBool] TVoid
    -- TODO setStyle
    -- TODO style
  , test (qtVersion < [5]) $ mkStaticMethod "syncX" [] TVoid
    -- TODO symbianEventFilter
    -- TODO symbianProcessEvent
  , just $ mkStaticMethod' "topLevelAt" "topLevelAtPoint" [TObj c_QPoint] $ TPtr $ TObj c_QWidget
  , just $ mkStaticMethod' "topLevelAt" "topLevelAtRaw" [TInt, TInt] $ TPtr $ TObj c_QWidget
    -- TODO topLevelWidgets
    -- We rename type() since @type@ is a Haskell keyword.
  , test (qtVersion < [5]) $ mkStaticMethod' "type" "applicationType" [] $ TEnum e_Type
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
    -- TODO quit (static!)
    -- TODO saveStateRequest
  ]

-- | Removed in Qt 5.
e_Type =
  makeQtEnum (ident1 "QApplication" "Type") [includeStd "QApplication"]
  [ (0, ["tty"])
  , (1, ["gui", "client"])
  , (2, ["gui", "server"])
  ]
