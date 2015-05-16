{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Widgets.QMenu (
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
#include "../Mk.hs.inc"

qtModule = makeQtModuleForClass c_QMenu $ map QtExportSignal signals

this = c_QMenu

c_QMenu =
  addReqIncludes [includeStd "QMenu"] $
  makeClass (ident "QMenu") Nothing
  [ c_QWidget ]
  [ _mkCtor "new" []
  , _mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  , _mkCtor "newWithTitle" [TObj c_QString]
  , _mkCtor "newWithTitleAndParent" [TObj c_QString, TPtr $ TObj c_QWidget]
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
  , _mkMethod' "exec" "exec" [] $ TPtr $ TObj c_QAction
  , _mkMethod' "exec" "execAt" [TObj c_QPoint, TPtr $ TObj c_QAction] $ TPtr $ TObj c_QAction
    -- TODO Static exec
  , _mkMethod "hideTearOffMenu" [] TVoid
  , _mkMethod "insertMenu" [TPtr $ TObj c_QAction, TPtr $ TObj c_QMenu] $ TPtr $ TObj c_QAction
  , _mkMethod "insertSeparator" [TPtr $ TObj c_QAction] $ TPtr $ TObj c_QAction
  , _mkConstMethod "isEmpty" [] TBool
  , _mkConstMethod "isTearOffMenuVisible" [] TBool
  , _mkConstMethod "menuAction" [] $ TPtr $ TObj c_QAction
  , _mkMethod' "popup" "popup" [TObj c_QPoint] TVoid
  , _mkMethod' "popup" "popupAction" [TObj c_QPoint, TPtr $ TObj c_QAction] TVoid
    -- TODO setIcon
  ] ++
  _props
  [ _mkProp "activeAction" $ TPtr $ TObj c_QAction
  , _mkProp "defaultAction" $ TPtr $ TObj c_QAction
    -- TODO icon
  ,_mkProp "separatorsCollapsible" TBool
  , _mkBoolIsProp "tearOffEnabled"
  , _mkProp "title" $ TObj c_QString
  ]

signals =
  [ _mkSignal "aboutToHide" c_Listener
  , _mkSignal "aboutToShow" c_Listener
  , _mkSignal "hovered" c_ListenerPtrQAction
  , _mkSignal "triggered" c_ListenerPtrQAction
  ]
