module Graphics.UI.Qtah.Internal.Interface.Widgets.QMenu (
  cppopModule,
  qtModule,
  c_QMenu,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Internal.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Listener (c_Listener, c_ListenerPtrQAction)
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.Widgets.QAction (c_QAction)
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

cppopModule = makeCppopModule "Widgets" "QMenu" qtModule

qtModule =
  makeQtModule "Widgets.QMenu" $
  QtExport (ExportClass c_QMenu) :
  map QtExportSignal signals

this = c_QMenu

c_QMenu =
  addReqIncludes [includeStd "QMenu"] $
  makeClass (ident "QMenu") Nothing
  [ c_QWidget ]
  [ mkCtor this "new" []
  , mkCtor this "newWithParent" [TPtr $ TObj c_QWidget]
  , mkCtor this "newWithTitle" [TObj c_QString]
  , mkCtor this "newWithTitleAndParent" [TObj c_QString, TPtr $ TObj c_QWidget]
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
  , mkMethod' this "exec" "exec" [] $ TPtr $ TObj c_QAction
  , mkMethod' this "exec" "execAt" [TObj c_QPoint, TPtr $ TObj c_QAction] $ TPtr $ TObj c_QAction
    -- TODO Static exec
  , mkMethod this "hideTearOffMenu" [] TVoid
  , mkMethod this "insertMenu" [TPtr $ TObj c_QAction, TPtr $ TObj c_QMenu] $ TPtr $ TObj c_QAction
  , mkMethod this "insertSeparator" [TPtr $ TObj c_QAction] $ TPtr $ TObj c_QAction
  , mkConstMethod this "isEmpty" [] TBool
  , mkConstMethod this "isTearOffMenuVisible" [] TBool
  , mkConstMethod this "menuAction" [] $ TPtr $ TObj c_QAction
  , mkMethod' this "popup" "popup" [TObj c_QPoint] TVoid
  , mkMethod' this "popup" "popupAction" [TObj c_QPoint, TPtr $ TObj c_QAction] TVoid
    -- TODO setIcon
  ] ++
  mkProps
  [ mkProp this "activeAction" $ TPtr $ TObj c_QAction
  , mkProp this "defaultAction" $ TPtr $ TObj c_QAction
    -- TODO icon
  ,mkProp this "separatorsCollapsible" TBool
  , mkBoolIsProp this "tearOffEnabled"
  , mkProp this "title" $ TObj c_QString
  ]

signals =
  [ makeSignal this "aboutToHide" c_Listener
  , makeSignal this "aboutToShow" c_Listener
  , makeSignal this "hovered" c_ListenerPtrQAction
  , makeSignal this "triggered" c_ListenerPtrQAction
  ]
