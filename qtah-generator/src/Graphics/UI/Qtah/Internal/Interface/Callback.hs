module Graphics.UI.Qtah.Internal.Interface.Callback where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)

mod_Callback =
  modifyModule' (makeModule "callback" "callback.hpp" "callback.cpp") $
  addModuleExports
  [ ExportCallback cb_BoolVoid
  , ExportCallback cb_IntIntVoid
  , ExportCallback cb_IntVoid
  , ExportCallback cb_PtrQObjectVoid
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

cb_IntVoid =
  makeCallback (toExtName "CallbackIntVoid")
  [TInt] TVoid

cb_PtrQObjectVoid =
  makeCallback (toExtName "CallbackPtrQObjectVoid")
  [TPtr $ TObj c_QObject] TVoid

cb_QStringVoid =
  makeCallback (toExtName "CallbackQStringVoid")
  [TObj c_QString] TVoid

cb_Void =
  makeCallback (toExtName "CallbackVoid")
  [] TVoid
