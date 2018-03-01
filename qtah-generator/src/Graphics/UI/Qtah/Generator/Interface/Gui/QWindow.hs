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

module Graphics.UI.Qtah.Generator.Interface.Gui.QWindow (
  minVersion,
  aModule,
  c_QWindow,
  e_Visibility,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass, ExportEnum),
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
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QMargins (c_QMargins)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (
  bs_WindowFlags,
  e_ScreenOrientation,
  e_WindowModality,
  e_WindowState,
  e_WindowType,
  qreal,
  )
import Graphics.UI.Qtah.Generator.Interface.Gui.QIcon (c_QIcon)
import Graphics.UI.Qtah.Generator.Interface.Gui.QRegion (c_QRegion)
import Graphics.UI.Qtah.Generator.Interface.Gui.QSurface (c_QSurface, e_SurfaceType)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  c_Listener,
  c_ListenerBool,
  c_ListenerInt,
  c_ListenerPtrQObject,
  c_ListenerQreal,
  c_ListenerQString,
  c_ListenerQWindowVisibility,
  c_ListenerScreenOrientation,
  c_ListenerWindowModality,
  c_ListenerWindowState,
  )
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

minVersion = [5, 0]

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Gui", "QWindow"] minVersion $
  [ QtExport $ ExportClass c_QWindow ] ++
  map QtExportSignal signals ++
  [ QtExport $ ExportEnum e_AncestorMode
  , QtExport $ ExportEnum e_Visibility
  ]

c_QWindow =
  addReqIncludes [includeStd "QWindow"] $
  classSetEntityPrefix "" $
  makeClass (ident "QWindow") Nothing [c_QObject, c_QSurface] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QWindow]
    -- TODO mkCtor "newWithScreen" [ptrT $ objT c_QScreen]
  , test (qtVersion >= [5, 1]) $ mkMethod "alert" [intT] voidT
  , just $ mkProp "baseSize" $ objT c_QSize
  , just $ mkMethod "close" [] voidT
  , just $ mkConstMethod "contentOrientation" [] $ enumT e_ScreenOrientation
  , just $ mkMethod "create" [] voidT
    -- TODO mkProp "cursor" $ objT c_QCursor
  , just $ mkMethod "destroy" [] voidT
  , just $ mkConstMethod "devicePixelRatio" [] qreal
  , just $ mkProp "filePath" $ objT c_QString
  , just $ mkProp "flags" $ bitspaceT bs_WindowFlags
  , just $ mkConstMethod "focusObject" [] $ ptrT $ objT c_QObject
  , just $ mkConstMethod "frameGeometry" [] $ objT c_QRect
  , just $ mkConstMethod "frameMargins" [] $ objT c_QMargins
  , just $ mkProp "framePosition" $ objT c_QPoint
  , just $ mkProp "geometry" $ objT c_QRect
  , just $ mkProp "height" intT
  , just $ mkMethod "hide" [] voidT
  , just $ mkProp "icon" $ objT c_QIcon
  , test (qtVersion >= [5, 1]) $ mkConstMethod "isActive" [] boolT
  , just $ mkConstMethod "isAncestorOf" [ptrT $ objT c_QWindow, enumT e_AncestorMode] boolT
  , just $ mkConstMethod "isExposed" [] boolT
  , just $ mkConstMethod "isModal" [] boolT
  , just $ mkConstMethod "isTopLevel" [] boolT
  , just $ mkMethod "lower" [] voidT
  , just $ mkConstMethod "mapFromGlobal" [objT c_QPoint] $ objT c_QPoint
  , just $ mkConstMethod "mapToGlobal" [objT c_QPoint] $ objT c_QPoint
  , just $ mkProp "mask" $ objT c_QRegion
  , just $ mkProp "maximumHeight" intT
  , just $ mkProp "maximumSize" $ objT c_QSize
  , just $ mkProp "maximumWidth" intT
  , just $ mkProp "minimumHeight" intT
  , just $ mkProp "minimumSize" $ objT c_QSize
  , just $ mkProp "minimumWidth" intT
  , just $ mkProp "modality" $ enumT e_WindowModality
  , test (qtVersion >= [5, 1]) $ mkProp "opacity" qreal
  , just $ mkProp "parent" $ ptrT $ objT c_QWindow
  , just $ mkProp "position" $ objT c_QPoint
  , just $ mkMethod "raise" [] voidT
  , just $ mkMethod "reportContentOrientationChange" [enumT e_ScreenOrientation] voidT
  , just $ mkMethod "requestActivate" [] voidT
  , test (qtVersion >= [5, 5]) $ mkMethod "requestUpdate" [] voidT
    -- TODO mkConstMethod "requestedFormat" [] $ objT c_QSurfaceFormat
  , just $ mkMethod' "resize" "resize" [objT c_QSize] voidT
  , just $ mkMethod' "resize" "resizeRaw" [intT, intT] voidT
    -- TODO mkProp "screen" $ ptrT $ objT c_QScreen
    -- TODO mkMethod "setFormat" [objT c_QSurfaceFormat] voidT
  , just $ mkMethod' "setGeometry" "setGeometryRaw" [intT, intT, intT, intT] voidT
  , just $ mkMethod "setKeyboardGrabEnabled" [boolT] voidT
  , just $ mkMethod "setMouseGrabEnabled" [boolT] voidT
  , just $ mkMethod' "setPosition" "setPositionRaw" [intT, intT] voidT
  , just $ mkMethod "setSurfaceType" [enumT e_SurfaceType] voidT
  , just $ mkMethod "show" [] voidT
  , just $ mkMethod "showFullScreen" [] voidT
  , just $ mkMethod "showMaximized" [] voidT
  , just $ mkMethod "showMinimized" [] voidT
  , just $ mkMethod "showNormal" [] voidT
  , just $ mkProp "sizeIncrement" $ objT c_QSize
  , just $ mkProp "title" $ objT c_QString
  , just $ mkProp "transientParent" $ ptrT $ objT c_QWindow
  , just $ mkConstMethod' "type" "getType" [] $ enumT e_WindowType
  , just $ mkMethod "unsetCursor" [] voidT
  , test (qtVersion >= [5, 1]) $ mkProp "visibility" $ enumT e_Visibility
  , just $ mkBoolIsProp "visible"
  , just $ mkProp "width" intT
    -- TODO winId
  , just $ mkProp "windowState" $ enumT e_WindowState
  , just $ mkProp "x" intT
  , just $ mkProp "y" intT
  ]

signals =
  [ makeSignal c_QWindow "activeChanged" c_Listener
  , makeSignal c_QWindow "contentOrientationChanged" c_ListenerScreenOrientation
  , makeSignal c_QWindow "focusObjectChanged" c_ListenerPtrQObject
  , makeSignal c_QWindow "heightChanged" c_ListenerInt
  , makeSignal c_QWindow "maximumHeightChanged" c_ListenerInt
  , makeSignal c_QWindow "maximumWidthChanged" c_ListenerInt
  , makeSignal c_QWindow "minimumHeightChanged" c_ListenerInt
  , makeSignal c_QWindow "minimumWidthChanged" c_ListenerInt
  , makeSignal c_QWindow "modalityChanged" c_ListenerWindowModality
  , makeSignal c_QWindow "opacityChanged" c_ListenerQreal
    -- TODO makeSignal c_QWindow "screenChanged" c_ListenerPtrQScreen
  , makeSignal c_QWindow "visibilityChanged" c_ListenerQWindowVisibility
  , makeSignal c_QWindow "visibleChanged" c_ListenerBool
  , makeSignal c_QWindow "widthChanged" c_ListenerInt
  , makeSignal c_QWindow "windowStateChanged" c_ListenerWindowState
  , makeSignal c_QWindow "windowTitleChanged" c_ListenerQString
  , makeSignal c_QWindow "xChanged" c_ListenerInt
  , makeSignal c_QWindow "yChanged" c_ListenerInt
  ]

e_AncestorMode =
  makeQtEnum (ident1 "QWindow" "AncestorMode") [includeStd "QWindow"]
  [ (0, ["exclude", "transients"])
  , (1, ["include", "transients"])
  ]

e_Visibility =
  makeQtEnum (ident1 "QWindow" "Visibility") [includeStd "QWindow"]
  [ (0, ["hidden"])
  , (1, ["automatic", "visibility"])
  , (2, ["windowed"])
  , (3, ["minimized"])
  , (4, ["maximized"])
  , (5, ["full", "screen"])
  ]
