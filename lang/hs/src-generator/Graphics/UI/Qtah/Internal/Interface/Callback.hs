module Graphics.UI.Qtah.Internal.Interface.Callback where

import Foreign.Cppop.Generator.Spec
import Foreign.Cppop.Generator.Std
import Graphics.UI.Qtah.Internal.Interface.QObject (c_QObject)

allCallbacks =
  [ cb_BoolVoid
  , cb_IntIntVoid
  , cb_IntVoid
  , cb_PtrQObjectVoid
  , cb_StringVoid
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

cb_StringVoid =
  makeCallback (toExtName "CallbackStringVoid")
  [TObj c_std__string] TVoid

f_testIntCallback =
  makeFn (ident "testIntCallback") (toExtName "testIntCallback") Nonpure
  [TCallback cb_IntVoid] TVoid

f_testStringCallback =
  makeFn (ident "testStringCallback") (toExtName "testStringCallback") Nonpure
  [TCallback cb_StringVoid] TVoid
