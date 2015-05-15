module Graphics.UI.Qtah.Internal.Interface.Callback where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Internal.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.Widgets.QAction (c_QAction)

mod_Callback =
  modifyModule' (makeModule "callback" "callback.hpp" "callback.cpp") $
  addModuleExports
  [ ExportCallback cb_BoolVoid
  , ExportCallback cb_IntIntVoid
  , ExportCallback cb_PtrQActionVoid
  , ExportCallback cb_PtrQObjectVoid
  , ExportCallback cb_QPointVoid
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

cb_QPointVoid =
  makeCallback (toExtName "CallbackQPointVoid")
  [TObj c_QPoint] TVoid

cb_QStringVoid =
  makeCallback (toExtName "CallbackQStringVoid")
  [TObj c_QString] TVoid

cb_Void =
  makeCallback (toExtName "CallbackVoid")
  [] TVoid
