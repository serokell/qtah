{-# LANGUAGE CPP #-}

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
#include "../Mk.hs.inc"

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
  [ _mkCtor "new" [TPtr $ TObj c_QWidget]
  , _mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  ] $
  [ _mkConstMethod "actionAt" [TObj c_QPoint] $ TPtr $ TObj c_QAction
  , _mkConstMethod "actionGeometry" [TPtr $ TObj c_QAction] $ TObj c_QRect
  , _mkMethod' "addAction" "addAction" [TPtr $ TObj c_QAction] TVoid
  , _mkMethod' "addAction" "addNewAction" [TObj c_QString] $ TPtr $ TObj c_QAction
    -- TODO addNewActionWithIcon and connecting forms
  , _mkMethod' "addMenu" "addMenu" [TPtr $ TObj c_QMenu] $ TPtr $ TObj c_QAction
  , _mkMethod' "addMenu" "addNewMenu" [TObj c_QString] $ TPtr $ TObj c_QMenu
    -- TODO addNewMenuWithIcon
  , _mkMethod "addSeparator" [] $ TPtr $ TObj c_QAction
  , _mkMethod "clear" [] TVoid
  , _mkConstMethod "cornerWidget" [TEnum e_Corner] $ TPtr $ TObj c_QWidget
  , _mkMethod "insertMenu" [TPtr $ TObj c_QAction, TPtr $ TObj c_QMenu] $ TPtr $ TObj c_QAction
  , _mkMethod "insertSeparator" [TPtr $ TObj c_QAction] $ TPtr $ TObj c_QAction
  , _mkMethod "setCornerWidget" [TPtr $ TObj c_QWidget, TEnum e_Corner] TVoid
  ] ++
  (_props . collect)
  [ just $ _mkProp "activeAction" $ TPtr $ TObj c_QAction
  , test wsWince $ _mkProp "defaultAction" $ TPtr $ TObj c_QAction
  , just $ _mkBoolIsProp "defaultUp"
  , just $ _mkBoolIsProp "nativeMenuBar"
  ]

signals =
  [ _mkSignal "hovered" c_ListenerPtrQAction
  , _mkSignal "triggered" c_ListenerPtrQAction
  ]
