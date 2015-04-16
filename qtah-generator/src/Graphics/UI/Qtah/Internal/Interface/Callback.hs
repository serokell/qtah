module Graphics.UI.Qtah.Internal.Interface.Callback where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Interface.QObject (c_QObject)
import Graphics.UI.Qtah.Internal.Interface.QString (c_QString)

allCallbacks =
  [ cb_BoolVoid
  , cb_IntIntVoid
  , cb_IntVoid
  , cb_PtrQObjectVoid
  , cb_QStringVoid
  , cb_Void
  ]

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
