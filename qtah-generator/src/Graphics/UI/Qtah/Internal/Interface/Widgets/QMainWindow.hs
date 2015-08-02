module Graphics.UI.Qtah.Internal.Interface.Widgets.QMainWindow (
  cppopModule,
  qtModule,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Internal.Interface.Listener (c_ListenerQSize)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QMenu (c_QMenu)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QMenuBar (c_QMenuBar)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

cppopModule = makeCppopModule "Widgets" "QMainWindow" qtModule

qtModule =
  makeQtModule "Widgets.QMainWindow" $
  QtExport (ExportClass c_QMainWindow) :
  map QtExportSignal signals

c_QMainWindow =
  addReqIncludes [includeStd "QMainWindow"] $
  makeClass (ident "QMainWindow") Nothing [c_QWidget]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
    -- TODO Ctor with Qt::WindowFlags.
  ] $
  [ -- TODO addDockWidget
    -- TODO addToolBar
    -- TODO addToolBarBreak
    -- TODO corner
    mkMethod "createPopupMenu" [] $ TPtr $ TObj c_QMenu
    -- TODO dockWidgetArea
    -- TODO insertToolBar
    -- TODO insertToolBarBreak
    -- TODO removeDockWidget
    -- TODO restoreState
    -- TODO saveState
    -- TODO setCorner
    -- TODO setTabPosition
    -- TODO setTabShape
    -- TODO splitDockWidget
    -- TODO tabifiedDockWidgets
    -- TODO tabifyDockWidget
    -- TODO tabPosition
    -- TODO tabShape
    -- TODO toolBarArea
    -- TODO toolBarBreak
  ] ++
  mkProps
  [ mkBoolIsProp "animated"
  , mkProp "centralWidget" $ TPtr $ TObj c_QWidget
  , mkBoolIsProp "dockNestingEnabled"
    -- TODO dockOptions
  , mkProp "documentMode" TBool
  , mkProp "iconSize" $ TObj c_QSize
  , mkProp "menuBar" $ TPtr $ TObj c_QMenuBar
  , mkProp "menuWidget" $ TPtr $ TObj c_QWidget
    -- TODO statusBar
    -- TODO tabShape
    -- TODO toolButtonStyle
  , mkProp "unifiedTitleAndToolBarOnMac" TBool
  ]

signals =
  [ makeSignal c_QMainWindow "iconSizeChanged" c_ListenerQSize
    -- TODO toolButtonStyleChanged
  ]
