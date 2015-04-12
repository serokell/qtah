module Graphics.UI.Qtah.Internal.Interface.Callback where

import Foreign.Cppop.Generator.Spec
import Foreign.Cppop.Generator.Std

cb_BoolVoid =
  makeCallback (toExtName "CallbackBoolVoid")
  [TBool] TVoid

cb_IntIntVoid =
  makeCallback (toExtName "CallbackIntIntVoid")
  [TInt, TInt] TVoid

cb_IntVoid =
  makeCallback (toExtName "CallbackIntVoid")
  [TInt] TVoid

cb_StringVoid =
  makeCallback (toExtName "CallbackStringVoid")
  [TObj cls_std__string] TVoid

f_testIntCallback =
  makeFn (ident "testIntCallback") (toExtName "testIntCallback") Nonpure
  [TCallback cb_IntVoid] TVoid

f_testStringCallback =
  makeFn (ident "testStringCallback") (toExtName "testStringCallback") Nonpure
  [TCallback cb_StringVoid] TVoid
