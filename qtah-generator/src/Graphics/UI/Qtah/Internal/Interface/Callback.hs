module Graphics.UI.Qtah.Internal.Interface.Callback where

import Foreign.Cppop.Generator.Spec (
  Export (ExportCallback),
  Type (TBool, TInt, TObj, TPtr, TVoid),
  addModuleExports,
  makeCallback,
  makeModule,
  modifyModule',
  toExtName,
  )
import Graphics.UI.Qtah.Internal.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Internal.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Internal.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.Widgets.QAction (c_QAction)
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

mod_Callback =
  modifyModule' (makeModule "callback" "b_callback.hpp" "b_callback.cpp") $
  addModuleExports
  [ ExportCallback cb_BoolVoid
  , ExportCallback cb_IntIntVoid
  , ExportCallback cb_PtrQActionVoid
  , ExportCallback cb_PtrQObjectVoid
  , ExportCallback cb_PtrQWidgetPtrQWidgetVoid
  , ExportCallback cb_QPointVoid
  , ExportCallback cb_QSizeVoid
  , ExportCallback cb_QStringVoid
  , ExportCallback cb_Void
  ]

qmods_Callback = []

cb_BoolVoid =
  makeCallback (toExtName "CallbackBoolVoid")
  [TBool] TVoid

cb_IntIntVoid =
  makeCallback (toExtName "CallbackIntIntVoid")
  [TInt, TInt] TVoid

cb_PtrQActionVoid =
  makeCallback (toExtName "CallbackPtrQActionVoid")
  [TPtr $ TObj c_QAction] TVoid

cb_PtrQObjectVoid =
  makeCallback (toExtName "CallbackPtrQObjectVoid")
  [TPtr $ TObj c_QObject] TVoid

cb_PtrQWidgetPtrQWidgetVoid =
  makeCallback (toExtName "CallbackPtrQWidgetPtrQWidgetVoid")
  [TPtr $ TObj c_QWidget, TPtr $ TObj c_QWidget] TVoid

cb_QPointVoid =
  makeCallback (toExtName "CallbackQPointVoid")
  [TObj c_QPoint] TVoid

cb_QSizeVoid =
  makeCallback (toExtName "CallbackQSizeVoid")
  [TObj c_QSize] TVoid

cb_QStringVoid =
  makeCallback (toExtName "CallbackQStringVoid")
  [TObj c_QString] TVoid

cb_Void =
  makeCallback (toExtName "CallbackVoid")
  [] TVoid
