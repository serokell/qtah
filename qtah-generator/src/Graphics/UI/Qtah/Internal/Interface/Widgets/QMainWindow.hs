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

this = c_QMainWindow

c_QMainWindow =
  addReqIncludes [includeStd "QMainWindow"] $
  makeClass (ident "QMainWindow") Nothing [c_QWidget]
  [ mkCtor this "new" []
  , mkCtor this "newWithParent" [TPtr $ TObj c_QWidget]
    -- TODO Ctor with Qt::WindowFlags.
  ] $
  [ -- TODO addDockWidget
    -- TODO addToolBar
    -- TODO addToolBarBreak
    -- TODO corner
    mkMethod this "createPopupMenu" [] $ TPtr $ TObj c_QMenu
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
  [ mkBoolIsProp this "animated"
  , mkProp this "centralWidget" $ TPtr $ TObj c_QWidget
  , mkBoolIsProp this "dockNestingEnabled"
    -- TODO dockOptions
  , mkProp this "documentMode" TBool
  , mkProp this "iconSize" $ TObj c_QSize
  , mkProp this "menuBar" $ TPtr $ TObj c_QMenuBar
  , mkProp this "menuWidget" $ TPtr $ TObj c_QWidget
    -- TODO statusBar
    -- TODO tabShape
    -- TODO toolButtonStyle
  , mkProp this "unifiedTitleAndToolBarOnMac" TBool
  ]

signals =
  [ makeSignal this "iconSizeChanged" c_ListenerQSize
    -- TODO toolButtonStyleChanged
  ]
