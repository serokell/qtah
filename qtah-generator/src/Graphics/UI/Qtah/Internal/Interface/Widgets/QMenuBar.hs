module Graphics.UI.Qtah.Internal.Interface.Widgets.QMenuBar (
  cppopModule,
  qtModule,
  c_QMenuBar,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Flag (collect, just, test)
import Graphics.UI.Qtah.Internal.Flags (wsWince)
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Internal.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (e_Corner)
import Graphics.UI.Qtah.Internal.Interface.Listener (c_ListenerPtrQAction)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QAction (c_QAction)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QMenu (c_QMenu)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

cppopModule = makeCppopModule "Widgets" "QMenuBar" qtModule

qtModule =
  makeQtModule "Widgets.QMenuBar" $
  QtExport (ExportClass c_QMenuBar) :
  map QtExportSignal signals

this = c_QMenuBar

c_QMenuBar =
  addReqIncludes [includeStd "QMenuBar"] $
  makeClass (ident "QMenuBar") Nothing
  [ c_QWidget ]
  [ mkCtor this "new" [TPtr $ TObj c_QWidget]
  , mkCtor this "newWithParent" [TPtr $ TObj c_QWidget]
  ] $
  [ mkConstMethod this "actionAt" [TObj c_QPoint] $ TPtr $ TObj c_QAction
  , mkConstMethod this "actionGeometry" [TPtr $ TObj c_QAction] $ TObj c_QRect
  , mkMethod' this "addAction" "addAction" [TPtr $ TObj c_QAction] TVoid
  , mkMethod' this "addAction" "addNewAction" [TObj c_QString] $ TPtr $ TObj c_QAction
    -- TODO addNewActionWithIcon and connecting forms
  , mkMethod' this "addMenu" "addMenu" [TPtr $ TObj c_QMenu] $ TPtr $ TObj c_QAction
  , mkMethod' this "addMenu" "addNewMenu" [TObj c_QString] $ TPtr $ TObj c_QMenu
    -- TODO addNewMenuWithIcon
  , mkMethod this "addSeparator" [] $ TPtr $ TObj c_QAction
  , mkMethod this "clear" [] TVoid
  , mkConstMethod this "cornerWidget" [TEnum e_Corner] $ TPtr $ TObj c_QWidget
  , mkMethod this "insertMenu" [TPtr $ TObj c_QAction, TPtr $ TObj c_QMenu] $ TPtr $ TObj c_QAction
  , mkMethod this "insertSeparator" [TPtr $ TObj c_QAction] $ TPtr $ TObj c_QAction
  , mkMethod this "setCornerWidget" [TPtr $ TObj c_QWidget, TEnum e_Corner] TVoid
  ] ++
  (mkProps . collect)
  [ just $ mkProp this "activeAction" $ TPtr $ TObj c_QAction
  , test wsWince $ mkProp this "defaultAction" $ TPtr $ TObj c_QAction
  , just $ mkBoolIsProp this "defaultUp"
  , just $ mkBoolIsProp this "nativeMenuBar"
  ]

signals =
  [ makeSignal this "hovered" c_ListenerPtrQAction
  , makeSignal this "triggered" c_ListenerPtrQAction
  ]
